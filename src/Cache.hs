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
module Cache where
import Clash.Prelude
import Control.Monad.State (State, execState)
import Control.Lens
import Control.Lens.TH (makeLenses)
import Debug.Trace (traceShowM, traceShowId, traceShow)

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

type ReadBuf = Vec 8 Inst
emptyReadBuf :: ReadBuf
emptyReadBuf = replicate d8 0

type ReadCount = Unsigned 4

bankIdx :: BankId -> Clash.Prelude.Index 8
bankIdx Bank0 = 0
bankIdx Bank1 = 1
bankIdx Bank2 = 2
bankIdx Bank3 = 3
bankIdx Bank4 = 4
bankIdx Bank5 = 5
bankIdx Bank6 = 6
bankIdx Bank7 = 7

data WayId = Way0
           | Way1
           deriving (Eq, Show, Enum, Generic, NFDataX)

wayIdx :: WayId -> Clash.Prelude.Index 2
wayIdx Way0 = 0
wayIdx Way1 = 1

type Set = BitVector 7

type CacheLoc = (WayId, BankId)

$(makeLenses ''CacheState)
$(makeLenses ''RunCacheState)
$(makeLenses ''CacheInput)
$(makeLenses ''CacheOutput)
$(makeLenses ''TransInfo)
$(makeLenses ''Trans)
$(makeLenses ''TransState)

withResetn :: ICache -> ICache
withResetn m i | i ^. resetn = m i
               | otherwise = cacheState .= emptyCacheState >> cacheOutput .= emptyCacheOutput

getInstFromLoc :: CacheInput -> CacheLoc -> Inst
getInstFromLoc CacheInput{..} (wid, bid) = _bankDataInput !! wid !! bid

genInstOutput :: CacheInput -> OutputState -> ICacheM ()
genInstOutput _ NoOutput = pure ()
genInstOutput _ (KnownOneInst i1) = cacheOutput . inst1 .= i1
genInstOutput _ (KnownTwoInst i1 i2) = cacheOutput . inst1 .= i1 >> cacheOutput . inst2 .= i2
genInstOutput i (ReadOneInst loc1) = cacheOutput . inst1 .= getInstFromLoc i loc1
genInstOutput i (ReadTwoInst loc1 loc2) = cacheOutput . inst1 .= getInstFromLoc i loc1
                                       >> cacheOutput . inst2 .= getInstFromLoc i loc2

withInstOutput :: ICache -> ICache
withInstOutput m i = use (cacheState . outputState) >>= genInstOutput i
                  >> cacheState . outputState .= NoOutput >> m i

data HitStatus = Miss
               | HitOneInst WayId BankId
               | HitTwoInst WayId BankId BankId
               deriving (Eq, Show, Generic, NFDataX)
checkHit :: CacheInput -> ICacheM HitStatus
checkHit i = f <$> hit0 <*> hit1
  where iaddr = i ^. instAddr
        itag = getAddrTag iaddr
        iset = getAddrSet iaddr
        ibank = getAddrBankId iaddr
        f False False = Miss
        f True _ | ibank == Bank7 = HitOneInst Way0 ibank
                 | otherwise = HitTwoInst Way0 ibank (succ ibank)
        f _ True | ibank == Bank7 = HitOneInst Way1 ibank
                 | otherwise = HitTwoInst Way1 ibank (succ ibank)
        hit0 = checkTag Way0 iset itag
        hit1 = checkTag Way1 iset itag

checkTag :: WayId -> Set -> Tag -> ICacheM Bool
checkTag way set t = (&&) <$> compareTag <*> isValid
  where isValid = flip testBit (fromEnum set) . (^?! _Just) <$> preuse (cacheState . validFile . ix (wayIdx way))
        getTag = (!! set) . (!! way) <$> use (cacheState . tagFile)
        compareTag = (== t) <$> getTag

setBankAddr :: CacheInput -> ICacheM ()
setBankAddr i = cacheOutput . bankAddrOutput .= getAddrSet (i ^. instAddr)

flipBit :: Bits a => Int -> a -> a
flipBit n x | b = clearBit x n
            | otherwise = setBit x n
  where b = testBit x n

lruSetter :: Bits a => WayId -> Int -> a -> a
lruSetter Way0 = flip setBit
lruSetter Way1 = flip clearBit

updateLRU :: WayId -> Set -> ICacheM ()
updateLRU way s = cacheState . lruFile . ix (fromEnum s) %= lruSetter way (fromEnum s)

getLRU :: Set -> ICacheM WayId
getLRU s = f . flip testBit (fromEnum s) . (^?! _Just) <$> preuse (cacheState . lruFile . ix (fromEnum s))
  where f True = Way1
        f False = Way0

outputHitStatus :: CacheInput -> HitStatus -> ICacheM ()
outputHitStatus i (HitOneInst wid bid) = setInstValid (True, False)
                                      >> setBankAddr i
                                      >> cacheState . outputState .= ReadOneInst (wid, bid)
                                      >> updateLRU wid (getAddrSet $ i ^. instAddr)
outputHitStatus i (HitTwoInst wid bid1 bid2) = setInstValid (True, True)
                                            >> setBankAddr i
                                            >> cacheState . outputState .= ReadTwoInst (wid, bid1) (wid, bid2)
                                            >> updateLRU wid (getAddrSet $ i ^. instAddr)
outputHitStatus i Miss = pure ()

-- If input addr requests a addr whose set is currently under operation
-- of a transaction, stall the pipeline
checkSetConflict :: CacheInput -> ICacheM Bool
checkSetConflict i = (&&) <$> f Way0 <*> f Way1
  where iset = getAddrSet $ i ^. instAddr
        getTransSet :: Trans -> Maybe Set
        getTransSet t | t ^. transState == TransAvailable = Nothing
                      | otherwise = Just . getAddrSet $ t ^. transAddr
        compareTransSet s trans = let mset = getTransSet trans
                                  in case mset of
                                       Nothing -> False
                                       Just x -> s == x
        f :: WayId -> ICacheM Bool
        f w = compareTransSet iset . (!! w) <$> use (cacheState . transInfo . trans)

-- If a refilling is taking place, forbid input
checkRefillConflict :: ICacheM Bool
checkRefillConflict = (&&) <$> f Trans0 <*> f Trans1
  where f tid = g . getTransState tid <$> use cacheState
        g (TransRefill _) = True
        g _ = False

data TransHitStatus = BufMiss
                    | BufHitOneInst Inst
                    | BufHitTwoInst Inst Inst

hitTrans :: Addr -> TransId -> Trans -> TransHitStatus
hitTrans addr tid Trans{..} | aset /= tset = BufMiss
                            | otherwise = f _transState
  where [aset, tset] = getAddrSet <$> [addr, _transAddr]
        reqBid = getAddrBankId addr
        f (TransRead bid rcnt rbuf) = hitRead (initialBankId bid rcnt) rcnt rbuf
        f (TransRefill rbuf) = hitFill rbuf
        f _ = BufMiss
        hitFill readBuf | reqBid == Bank7 = BufHitOneInst $ readBuf !! fromEnum reqBid
                        | otherwise = BufHitTwoInst (readBuf !! fromEnum reqBid) $ readBuf ^?! ix (bankIdx (succ reqBid))
        hitRead initialBid readCount readBuf | readCount <= reqReadCount = BufMiss
                                             | readCount - 1 == reqReadCount = BufHitOneInst $ readBuf !! fromEnum reqBid
                                             | otherwise = BufHitTwoInst (readBuf ^?! ix (bankIdx reqBid)) $ readBuf ^?! ix (bankIdx (succ reqBid))
         where reqReadCount = readCountBetween initialBid reqBid

checkHitTrans :: Addr -> TransInfo -> TransHitStatus
checkHitTrans addr info = f (g Trans0) (g Trans1)
  where g tid = hitTrans addr tid $ info ^?! trans . ix (transIdx tid)
        f BufMiss BufMiss = BufMiss
        f BufMiss y = y
        f x _ = x

setInstValid :: (Bool, Bool) -> ICacheM ()
setInstValid (x, y) = cacheOutput . inst1Valid .= x >> cacheOutput . inst2Valid .= y

outputTransHitStatus :: TransHitStatus -> ICacheM ()
outputTransHitStatus BufMiss = setInstValid (False, False)
outputTransHitStatus (BufHitOneInst i1) = setInstValid (True, False) >> cacheState . outputState .= KnownOneInst i1
outputTransHitStatus (BufHitTwoInst i1 i2) = setInstValid (True, True) >> cacheState . outputState .= KnownTwoInst i1 i2

initialBankId :: BankId -> ReadCount -> BankId
initialBankId bid 0 = bid
initialBankId bid x = initialBankId (predBank bid) (x - 1)

readCountBetween :: BankId -> BankId -> ReadCount
readCountBetween b1 b2 | b1 == b2 = 0
                       | otherwise = readCountBetween b1 (predBank b2) + 1

predBank :: BankId -> BankId
predBank Bank0 = Bank7
predBank x = pred x

data AxiAddr = AxiAddr { _axiRid :: BitVector 4
                       , _axiRAddr :: Addr
                       } deriving (Eq, Show, Generic, NFDataX)

getAxiAddrOfTrans :: TransId -> Trans -> Maybe AxiAddr
getAxiAddrOfTrans tid Trans{..} | _transState == TransAddressing = let _axiRid = getRidOfTransId tid
                                                                       _axiRAddr = _transAddr
                                                                   in Just AxiAddr{..}
                                | otherwise = Nothing

getAxiAddrOfTransInfo :: TransInfo -> Maybe AxiAddr
getAxiAddrOfTransInfo TransInfo{..} | _priorTrans == Trans0 = a1 <|> a2
                                    | _priorTrans == Trans1 = a2 <|> a1
  where a1 = getAxiAddrOfTrans Trans0 $ _trans !! Trans0
        a2 = getAxiAddrOfTrans Trans1 $ _trans !! Trans1

genAxiAddrOutput :: TransInfo -> ICacheM ()
genAxiAddrOutput info = go $ getAxiAddrOfTransInfo info
  where go :: Maybe AxiAddr -> ICacheM ()
        go Nothing = pure ()
        go (Just AxiAddr{..}) = cacheOutput . arid .= _axiRid
                             >> cacheOutput . araddr .= _axiRAddr
                             >> cacheOutput . arvalid .= True

traceM :: (Show a, Applicative m, Monad m) => a -> m a
traceM x = traceShowM x >> pure x

updateTransWithAxiAddrReady :: Trans -> Trans
updateTransWithAxiAddrReady t@Trans{..} | _transState == TransAddressing = t & transState .~ TransRead (getAddrBankId $ t ^. transAddr) 0 emptyReadBuf
                                        | otherwise = t

handleAxiAddrReady :: Bool -> CacheState -> ICacheM ()
handleAxiAddrReady False _ = pure ()
handleAxiAddrReady True CacheState{..} = cacheState . transInfo . trans .= fmap updateTransWithAxiAddrReady (_transInfo ^. trans)

genAxiRReady :: TransInfo -> ICacheM ()
genAxiRReady info = cacheOutput . rready .= (f Trans0 || f Trans1)
  where f :: TransId -> Bool
        f tid = g $ (info ^. trans) !! tid ^. transState
        g (TransRead _ _ _) = True
        g _ = False

updateTransWithAxiData :: BankId -- current bank id
                       -> ReadCount -- current read count
                       -> ReadBuf -- current bank buf
                       -> Inst -- input rdata
                       -> Bool -- input rlast
                       -> Trans -- current trans
                       -> Trans -- next trans
updateTransWithAxiData bid rcnt buf axiRData axiRLast = f rcnt axiRLast
  where f 7 True = over transState toRefill . g
        f _ _ = g
        toRefill (TransRead _ _ buf) = TransRefill buf
        g = transState .~ (TransRead (succBank bid) (rcnt + 1) $ buf & ix (bankIdx bid) .~ axiRData)

-- A safe version of succ for BankId, with wrap around
succBank :: BankId -> BankId
succBank Bank7 = Bank0
succBank x = succ x

transIdx :: TransId -> Clash.Prelude.Index 2
transIdx Trans0 = 0
transIdx Trans1 = 1

handleAxiData :: CacheInput -> CacheState -> ICacheM ()
handleAxiData CacheInput{..} cs | not _rvalid = pure ()
                                | otherwise = f $ getTransState tid cs
  where f :: TransState -> ICacheM ()
        f (TransRead rbank rcnt rbuf) = cacheState . transInfo . trans . ix (transIdx tid) .= updateTransWithAxiData rbank rcnt rbuf _rdata _rlast tr
        f _ = pure () -- this should NEVER happen!
        tid = getTransIdOfRid _rid
        tr = cs ^?! transInfo . trans . ix (transIdx tid)

data RefillData = RefillData { refillAddr :: Set
                             , refillData :: WayBankData
                             } deriving (Eq, Show, Generic, NFDataX)

getRefillDataOfTrans :: Trans -> Maybe RefillData
getRefillDataOfTrans t = g $ t ^. transState
  where g (TransRefill buf) = let refillAddr = tset
                                  refillData = buf
                              in Just RefillData{..}
        g _ = Nothing
        tset = getAddrSet $ t ^. transAddr

outputRefillData :: WayId -> RefillData -> ICacheM ()
outputRefillData wid RefillData{..} = cacheOutput . bankAddrOutput .= refillAddr
                                   >> cacheOutput . bankDataOutput .= refillData
                                   >> cacheOutput . bankWriteEnableOutput . ix (wayIdx wid) .= ($$(bLit "11111111") :: BitVector 8)
                                   >> updateLRU wid refillAddr

resetTrans :: TransId -> ICacheM ()
resetTrans tid = cacheState . transInfo . trans . ix (transIdx tid) .= emptyTrans

genRefillOutput :: TransInfo -> ICacheM ()
genRefillOutput info = h $ g prior (f Trans0) (f Trans1)
  where f :: TransId -> Maybe (TransId, WayId, RefillData)
        f tid = fmap (tid, info ^?! trans . ix (transIdx tid) . fillWay,) $ getRefillDataOfTrans $ info ^?! trans . ix (transIdx tid)
        prior = info ^. priorTrans
        g Trans0 a b = a <|> b
        g Trans1 a b = b <|> a
        h Nothing = pure ()
        h (Just (tid, wid, d)) = outputRefillData wid d >> resetTrans tid

getTrans :: TransId -> CacheState -> Trans
getTrans tid cs = (cs ^. transInfo . trans) !! tid

getTransState :: TransId -> CacheState -> TransState
getTransState = fmap (view transState) . getTrans

getAddrTag :: Addr -> Tag
getAddrTag = slice d31 d12

getAddrSet :: Addr -> Set
getAddrSet = slice d11 d5

getAddrBankId :: Addr -> BankId
getAddrBankId = toEnum . fromEnum . slice d4 d2

getFreeTrans :: ICacheM (Maybe TransId)
getFreeTrans = liftA2 (<|>) (f Trans0) (f Trans1)
  where f tid = (== TransAvailable) . getTransState tid <$> use cacheState >>= h tid
        h tid True = pure $ Just tid
        h _ False = pure $ Nothing

handleRequest :: ICache
handleRequest i = use (cacheState . request) >>= f
  where f Nothing = withAlignCheck handleInputRequest i -- handle input
        f (Just addr) = checkHitTrans addr <$> use (cacheState . transInfo) >>= g addr
        g _ BufMiss = pure () -- buffered request miss, do nothing
        g addr status | i ^. instAddr == addr = outputTransHitStatus status >> cacheState . request .= Nothing
                      | otherwise = cacheState . request .= Nothing -- clear request, do nothing

handleInputRequest :: ICache
handleInputRequest i = checkHitTrans (i ^. instAddr) <$> use (cacheState . transInfo) >>= f
  where f BufMiss = withSetConflictCheck (withRefillingCheck runHitCheck) i
        f status = outputTransHitStatus status -- output inst
        runHitCheck i = checkHit i >>= g
        g Miss = handleInputMiss i -- input miss
        g status = outputHitStatus i status

withAlignCheck :: ICache -> ICache
withAlignCheck m i = g $ f (i ^. instAddr)
  where f :: BitVector 32 -> Bool
        f x = fromEnum (slice d1 d0 x) == 0
        g True = m i
        g False = setInstValid (True, True) >> cacheState . outputState .= KnownTwoInst 0 0

withSetConflictCheck :: ICache -> ICache
withSetConflictCheck m i = checkSetConflict i >>= f
  where f True = pure ()
        f False = m i

withRefillingCheck :: ICache -> ICache
withRefillingCheck m i = checkRefillConflict >>= f
  where f True = pure ()
        f False = m i

handleInputMiss :: ICache
handleInputMiss i = getFreeTrans >>= f
  where f Nothing = pure ()
        f (Just tid) = submitTrans tid (i ^. instAddr)

submitTrans :: TransId -> Addr -> ICacheM ()
submitTrans tid addr = f >>=
                           \way -> cacheState . transInfo . trans . ix (transIdx tid) .= emptyTrans { _transAddr = addr, _fillWay = way, _transState = TransAddressing }
                                >> cacheState . transInfo . priorTrans .= tid
                                >> cacheState . request .= Just (traceShow addr addr)
                                >> (use (cacheState . transInfo) >>= traceShowM)
  where f = getLRU (getAddrSet addr)

handleTrans :: CacheState -> ICache
handleTrans cs i = handleAxiAddrReady (i ^. arready) cs >> genAxiAddrOutput (cs ^. transInfo)
                >> genAxiRReady (cs ^. transInfo) >> handleAxiData i cs >> genRefillOutput (cs ^. transInfo)

icacheS :: ICache
icacheS = withResetn . withInstOutput $ \i -> do
  current <- use cacheState
  handleRequest i
  handleTrans current i

icacheT :: CacheState -> CacheInput -> (CacheState, CacheOutput)
icacheT s i = f $ execState (icacheS i) emptyRunCacheState { _cacheState = s }
  where f RunCacheState{..} = (_cacheState, _cacheOutput)

-- >>> cs = emptyCacheState
-- >>> i = CacheInput 0 True False 0 0 False False $ replicate d2 $ replicate d8 0
-- >>> ci = i { _instAddr = 1000 }
-- >>> cs' = (icacheT cs ci ^. _1) & outputState .~ KnownOneInst 123
-- >>> ci' = CacheInput 1 True False 0 0 False False $ replicate d2 $ replicate d8 0
-- >>> cs'' = icacheT cs' ci' ^. _1
-- >>> ci'' = i { _arready = True, _rvalid = True, _rid = 0, _rdata = 12321 }
-- >>> f x y = icacheT y x ^. _1
-- >>> g = f ci''
-- >>> cs''' = g . g . g . g $ cs''
-- >>> cs''' ^?! transInfo . trans
-- >>> cs''' ^?! request
-- TransInfo {_priorTrans = Trans0, _trans = <Trans {_transAddr = 0000_0000_0000_0000_0000_0011_1110_1000, _transState = TransAddressing, _fillWay = Way0},Trans {_transAddr = 0000_0000_0000_0000_0000_0000_0000_0000, _transState = TransAvailable, _fillWay = Way0}>}
-- 0000_0000_0000_0000_0000_0011_1110_1000
-- TransInfo {_priorTrans = Trans1, _trans = <Trans {_transAddr = 0000_0000_0000_0000_0000_0011_1110_1000, _transState = TransRead Bank4 2 <0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0011_0000_0010_0001,0000_0000_0000_0000_0011_0000_0010_0001,0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0000_0000_0000_0000>, _fillWay = Way0},Trans {_transAddr = 0000_0000_0000_0000_0000_0000_0000_0000, _transState = TransAddressing, _fillWay = Way0}>}
-- <Trans {_transAddr = 0000_0000_0000_0000_0000_0011_1110_1000, _transState = TransRead Bank5 3 <0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0011_0000_0010_0001,0000_0000_0000_0000_0011_0000_0010_0001,0000_0000_0000_0000_0011_0000_0010_0001,0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0000_0000_0000_0000,0000_0000_0000_0000_0000_0000_0000_0000>, _fillWay = Way0},Trans {_transAddr = 0000_0000_0000_0000_0000_0000_0000_0000, _transState = TransAvailable, _fillWay = Way0}>
-- 0000_0000_0000_0000_0000_0000_0000_0000
-- Just 0000_0000_0000_0000_0000_0000_0000_0000
--

{-# ANN topEntity
  (Synthesize
    { t_name   = "icache_controller"
    , t_inputs = [ PortName "clock"
                 , PortName "__reset"
                 , PortName "__en"
                 , PortProduct "" [ PortName "inst_addr_i"
                                  , PortName "resetn"
                                  , PortName "arready_i"
                                  , PortName "rid_i"
                                  , PortName "rdata_i"
                                  , PortName "rlast_i"
                                  , PortName "rvalid_i"
                                  , PortProduct "bank_data_i" []
                                  ]
                 ]
    , t_output = PortProduct "" [ PortName "inst_1_o"
                                , PortName "inst_2_o"
                                , PortName "inst_1_valid"
                                , PortName "inst_2_valid"
                                , PortName "axi_arid_o"
                                , PortName "axi_araddr_o"
                                , PortName "axi_arvalid_o"
                                , PortName "axi_rready_o"
                                , PortName "bank_set_addr_o"
                                , PortProduct "bank_data_o" []
                                , PortProduct "bank_wen_o" []
                                ]
    }) #-}
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System CacheInput
  -> Signal System CacheOutput
topEntity = exposeClockResetEnable icache
  where icache = mealy icacheT emptyCacheState
