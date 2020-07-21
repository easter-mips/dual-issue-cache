{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module ICache.Types where
import Clash.Prelude
import Control.Monad.State (State)
import Control.Lens
import Control.Lens.TH (makeLenses)

type ICacheM = State RunCacheState

type ICache = CacheInput -> ICacheM ()

data CacheState = CacheState { _outputState :: OutputState
                             , _validFile :: ValidFile
                             , _tagFile :: TagFile
                             , _lruFile :: LruFile
                             , _request :: Maybe Addr
                             , _transInfo :: TransInfo
                             } deriving (Eq, Show, Generic, NFDataX)

data RunCacheState = RunCacheState { _cacheState :: CacheState
                                   , _cacheOutput :: CacheOutput
                                   } deriving (Eq, Show, Generic)

emptyCacheState :: CacheState
emptyCacheState = CacheState{..}
  where _outputState = NoOutput
        _validFile = replicate d2 0
        _tagFile = replicate d2 $ replicate d128 0
        _lruFile = 0
        _request = Nothing
        _transInfo = emptyTransInfo

emptyRunCacheState :: RunCacheState
emptyRunCacheState = RunCacheState{..}
  where _cacheState = emptyCacheState
        _cacheOutput = emptyCacheOutput

data CacheInput = CacheInput { _instAddr :: Addr
                             , _resetn :: Bool

                             -- Read address channel inputs
                             , _arready :: Bool

                             -- Read data channel inputs
                             , _rid :: BitVector 4
                             , _rdata :: Inst
                             , _rlast :: Bool
                             , _rvalid :: Bool

                             -- bank ram inputs
                             , _bankDataInput :: BankData
                             } deriving (Eq, Show, Generic, NFDataX)

type WayBankData = Vec 8 Inst
type BankData = Vec 2 WayBankData
type WayBankWriteEnableOutput = BitVector 8
type BankWriteEnableOutput = Vec 2 WayBankWriteEnableOutput

data CacheOutput = CacheOutput { _inst1 :: Inst
                               , _inst2 :: Inst
                               , _inst1Valid :: Bool
                               , _inst2Valid :: Bool

                               -- Read address channel outputs
                               , _arid :: BitVector 4
                               , _araddr :: Addr
                               , _arvalid :: Bool

                               -- Read data channel outputs
                               , _rready :: Bool

                               -- Bank data outputs
                               , _bankAddrOutput :: Set
                               , _bankDataOutput :: WayBankData
                               , _bankWriteEnableOutput :: BankWriteEnableOutput
                               } deriving (Eq, Show, Generic, NFDataX)

emptyCacheOutput :: CacheOutput
emptyCacheOutput = CacheOutput{..}
  where _inst1 = 0
        _inst2 = 0
        _inst1Valid = False
        _inst2Valid = False
        _arid = 0
        _araddr = 0
        _arvalid = False
        _rready = False
        _bankAddrOutput = 0
        _bankDataOutput = replicate d8 0
        _bankWriteEnableOutput = replicate d2 0

data OutputState = NoOutput
                 | KnownOneInst Inst
                 | KnownTwoInst Inst Inst
                 | ReadOneInst CacheLoc
                 | ReadTwoInst CacheLoc CacheLoc
                 deriving (Eq, Show, Generic, NFDataX)

type Inst = BitVector 32
type Addr = BitVector 32

type WayValidFile = BitVector 128
type ValidFile = Vec 2 WayValidFile

type Tag = BitVector 20
type WayTagFile = Vec 128 Tag
type TagFile = Vec 2 WayTagFile

type LruFile = BitVector 128

type Request = Maybe Addr

data TransInfo = TransInfo { _priorTrans :: TransId
                           , _trans :: Vec 2 Trans
                           } deriving (Eq, Show, Generic, NFDataX)

emptyTransInfo = TransInfo{..}
  where _priorTrans = Trans0
        _trans = replicate d2 emptyTrans

emptyTrans = Trans{..}
  where _transAddr = 0
        _transState = TransAvailable
        _transBuf = replicate d8 0
        _fillWay = Way0
        

data TransId = Trans0 | Trans1
             deriving (Eq, Show, Enum, Generic, NFDataX)

data Trans = Trans { _transAddr :: Addr
                   , _transState :: TransState
                   , _fillWay :: WayId
                   } deriving (Eq, Show, Generic, NFDataX)

getRidOfTransId :: TransId -> BitVector 4
getRidOfTransId Trans0 = $$(bLit "0000")
getRidOfTransId Trans1 = $$(bLit "0001")

getTransIdOfRid :: BitVector 4 -> TransId
getTransIdOfRid $(bitPattern "0000") = Trans0
getTransIdOfRid $(bitPattern "0001") = Trans1

data TransState = TransAvailable
                | TransAddressing
                | TransRead BankId ReadCount ReadBuf
                | TransRefill ReadBuf
                deriving (Eq, Show, Generic, NFDataX)

data BankId = Bank0
            | Bank1
            | Bank2
            | Bank3
            | Bank4
            | Bank5
            | Bank6
            | Bank7
            deriving (Eq, Show, Enum, Generic, NFDataX)

data AxiAddr = AxiAddr { _axiRid :: BitVector 4
                       , _axiRAddr :: Addr
                       } deriving (Eq, Show, Generic, NFDataX)

getAddrTag :: Addr -> Tag
getAddrTag = slice d31 d12

getAddrSet :: Addr -> Set
getAddrSet = slice d11 d5

getAddrBankId :: Addr -> BankId
getAddrBankId = toEnum . fromEnum . slice d4 d2

-- convert bank id to clash fixed-size index
bankIdx :: BankId -> Clash.Prelude.Index 8
bankIdx Bank0 = 0
bankIdx Bank1 = 1
bankIdx Bank2 = 2
bankIdx Bank3 = 3
bankIdx Bank4 = 4
bankIdx Bank5 = 5
bankIdx Bank6 = 6
bankIdx Bank7 = 7

type ReadBuf = Vec 8 Inst
emptyReadBuf :: ReadBuf
emptyReadBuf = replicate d8 0

type ReadCount = Unsigned 3

data WayId = Way0
           | Way1
           deriving (Eq, Show, Enum, Generic, NFDataX)

wayIdx :: WayId -> Clash.Prelude.Index 2
wayIdx Way0 = 0
wayIdx Way1 = 1

type Set = BitVector 7

type CacheLoc = (WayId, BankId)

data TransHitStatus = BufMiss
                    | BufHitOneInst Inst
                    | BufHitTwoInst Inst Inst

data HitStatus = Miss
               | HitOneInst WayId BankId
               | HitTwoInst WayId BankId BankId
               deriving (Eq, Show, Generic, NFDataX)

-- A safe version of succ for BankId, with wrap around
succBank :: BankId -> BankId
succBank Bank7 = Bank0
succBank x = succ x

transIdx :: TransId -> Clash.Prelude.Index 2
transIdx Trans0 = 0
transIdx Trans1 = 1

$(makeLenses ''CacheState)
$(makeLenses ''RunCacheState)
$(makeLenses ''CacheInput)
$(makeLenses ''CacheOutput)
$(makeLenses ''TransInfo)
$(makeLenses ''Trans)
$(makeLenses ''TransState)

getTrans :: TransId -> CacheState -> Trans
getTrans tid cs = (cs ^. transInfo . trans) !! tid

getTransState :: TransId -> CacheState -> TransState
getTransState = fmap (view transState) . getTrans
