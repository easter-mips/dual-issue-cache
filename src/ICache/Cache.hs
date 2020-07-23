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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module ICache.Cache where
import Clash.Prelude
import Control.Lens
import Control.Monad.State (execState)
import ICache.Types
-- import Utils.Debug -- trace functions

icacheT :: CacheState -> CacheInput -> (CacheState, CacheOutput)
icacheT s i = f $ execState (icacheS i) emptyRunCacheState { _cacheState = s }
  where f RunCacheState{..} = (_cacheState, _cacheOutput)

icacheS :: ICache
icacheS = withResetn . withInstOutput $ \i -> do
  handleRequest i
  current <- use cacheState
  handleTrans current i

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

checkHit :: CacheInput -> ICacheM HitStatus
checkHit i = f <$> hit0 <*> hit1
  where iaddr = i ^. instAddr
        itag = getAddrTag iaddr
        is = getAddrSet iaddr
        ibank = getAddrBankId iaddr
        f False False = Miss
        f True _ | ibank == Bank7 = HitOneInst Way0 ibank
                 | otherwise = HitTwoInst Way0 ibank (succ ibank)
        f _ True | ibank == Bank7 = HitOneInst Way1 ibank
                 | otherwise = HitTwoInst Way1 ibank (succ ibank)
        hit0 = checkTag Way0 is itag
        hit1 = checkTag Way1 is itag

checkTag :: WayId -> Set -> Tag -> ICacheM Bool
checkTag way s t = (&&) <$> compareTag <*> isValid
  where isValid = flip testBit (fromEnum s) . (^?! _Just) <$> preuse (cacheState . validFile . ix (wayIdx way))
        getTag = (!! s) . (!! way) <$> use (cacheState . tagFile)
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
outputHitStatus i (HitOneInst wid bid) = setInstValid (True, False) -- state change: LRU
                                      >> setBankAddr i
                                      >> cacheState . outputState .= ReadOneInst (wid, bid)
                                      >> updateLRU wid (getAddrSet $ i ^. instAddr)
outputHitStatus i (HitTwoInst wid bid1 bid2) = setInstValid (True, True)
                                            >> setBankAddr i
                                            >> cacheState . outputState .= ReadTwoInst (wid, bid1) (wid, bid2)
                                            >> updateLRU wid (getAddrSet $ i ^. instAddr)
outputHitStatus _ Miss = pure ()

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
checkRefillConflict = (||) <$> f Trans0 <*> f Trans1
  where f :: TransId -> ICacheM Bool
        f tid = g . getTransState tid <$> use cacheState
        g (TransRefill _) = True
        g _ = False

hitTrans :: Addr -> TransId -> Trans -> TransHitStatus
hitTrans addr tid Trans{..} | aset /= tset || atag /= ttag = BufMiss
                            | otherwise = f _transState
  where [aset, tset] = getAddrSet <$> [addr, _transAddr]
        [atag, ttag] = getAddrTag <$> [addr, _transAddr]
        reqBid = getAddrBankId addr
        f (TransRead bid rcnt rbuf) | reqBid == Bank7 = BufMiss
                                    | otherwise = hitRead (initialBankId bid rcnt) rcnt rbuf
        f (TransRefill rbuf) = hitFill rbuf
        f _ = BufMiss
        hitFill readBuf | reqBid == Bank7 = BufHitOneInst $ readBuf !! fromEnum reqBid
                        | otherwise = BufHitTwoInst (readBuf !! fromEnum reqBid) $ readBuf ^?! ix (bankIdx (succ reqBid))
        hitRead initialBid readCount readBuf | readCount <= reqReadCount = BufMiss
                                             | readCount - 1 == reqReadCount = BufHitOneInst $ readBuf !! reqBid
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
initialBankId bid x = sigToBid $ bidToSig bid - x

bidToSig :: BankId -> Unsigned 3
bidToSig = toEnum . fromEnum

sigToBid :: Unsigned 3 -> BankId
sigToBid = toEnum . fromEnum

readCountBetween :: BankId -> BankId -> ReadCount
readCountBetween b1 b2 = bidToSig b2 - bidToSig b1

predBank :: BankId -> BankId
predBank Bank0 = Bank7
predBank x = pred x

getAxiAddrOfTrans :: TransId -> Trans -> Maybe AxiAddr
getAxiAddrOfTrans tid Trans{..} | _transState == TransAddressing = let _axiRid = getRidOfTransId tid
                                                                       _axiRAddr = _transAddr
                                                                   in Just AxiAddr{..}
                                | otherwise = Nothing

getAxiAddrOfTransInfo :: TransInfo -> Maybe AxiAddr
getAxiAddrOfTransInfo TransInfo{..} | _priorTrans == Trans0 = a1 <|> a2
                                    | _priorTrans == Trans1 = a2 <|> a1
                                    | otherwise = error "invalid prior trans" -- this should NEVER happen
  where a1 = getAxiAddrOfTrans Trans0 $ _trans !! Trans0
        a2 = getAxiAddrOfTrans Trans1 $ _trans !! Trans1

genAxiAddrOutput :: TransInfo -> ICacheM ()
genAxiAddrOutput info = go $ getAxiAddrOfTransInfo info
  where go :: Maybe AxiAddr -> ICacheM ()
        go Nothing = pure ()
        go (Just AxiAddr{..}) = cacheOutput . arid .= _axiRid
                             >> cacheOutput . araddr .= _axiRAddr
                             >> cacheOutput . arvalid .= True

updateTransWithAxiAddrReady :: Trans -> Trans
updateTransWithAxiAddrReady t@Trans{..} | _transState == TransAddressing = t & transState .~ TransRead (getAddrBankId $ t ^. transAddr) 0 emptyReadBuf
                                        | otherwise = t

handleAxiAddrReady :: Bool -> CacheState -> ICacheM ()
handleAxiAddrReady False _ = pure ()
handleAxiAddrReady True CacheState{..} = cacheState . transInfo . trans
                                         .= fmap updateTransWithAxiAddrReady (_transInfo ^. trans)

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
        toRefill _ = error "ToRefill called on non-read state" -- this will NEVER happen
        g = transState .~ (TransRead (succBank bid) (rcnt + 1) $ buf & ix (bankIdx bid) .~ axiRData)

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
                             , refillTag :: Tag
                             } deriving (Eq, Show, Generic, NFDataX)

getRefillDataOfTrans :: Trans -> Maybe RefillData
getRefillDataOfTrans t = g $ t ^. transState
  where g (TransRefill buf) = let refillAddr = tset
                                  refillData = buf
                                  refillTag = getAddrTag $ t ^. transAddr
                              in Just RefillData{..}
        g _ = Nothing
        tset = getAddrSet $ t ^. transAddr

outputRefillData :: WayId -> RefillData -> ICacheM ()
outputRefillData wid RefillData{..} = cacheOutput . bankAddrOutput .= refillAddr
                                   >> cacheOutput . bankDataOutput .= refillData
                                   >> cacheOutput . bankWriteEnableOutput . ix (wayIdx wid)
                                      .= ($$(bLit "11111111") :: BitVector 8)
                                   >> updateLRU wid refillAddr
                                   >> updateValid wid refillAddr
                                   >> updateTag wid refillAddr refillTag

updateValid :: WayId -> Set -> ICacheM ()
updateValid w s = cacheState . validFile . ix (wayIdx w) %= flip setBit (fromEnum s)

updateTag :: WayId -> Set -> Tag -> ICacheM ()
updateTag w s t = cacheState . tagFile . ix (wayIdx w) %= replace s t

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

getFreeTrans :: ICacheM (Maybe TransId)
getFreeTrans = liftA2 (<|>) (f Trans0) (f Trans1)
  where f :: TransId -> ICacheM (Maybe TransId)
        f tid = (== TransAvailable) . getTransState tid <$> use cacheState >>= h tid
        h tid True = pure $ Just tid
        h _ False = pure $ Nothing

handleRequest :: ICache
handleRequest i = use (cacheState . request) >>= f
  where f Nothing = withAlignCheck handleInputRequest i -- handle input
        f (Just addr) = checkHitTrans addr <$> use (cacheState . transInfo) >>= g addr
        g _ BufMiss = pure () >> outputStallReason normalMiss -- buffered request miss, do nothing
        g addr status | i ^. instAddr == addr = outputTransHitStatus status >> cacheState . request .= Nothing
                      | otherwise = cacheState . request .= Nothing -- clear request, do nothing
                                                                    -- state change: requested
handleInputRequest :: ICache
handleInputRequest i = checkHitTrans (i ^. instAddr) <$> use (cacheState . transInfo) >>= f
  where f BufMiss = withSetConflictCheck (withRefillingCheck runHitCheck) i -- check hit bank
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
  where f True = pure () >> outputStallReason setConflict
        f False = m i

withRefillingCheck :: ICache -> ICache
withRefillingCheck m i = checkRefillConflict >>= f
  where f True = pure () >> outputStallReason refillConflict
        f False = m i

handleInputMiss :: ICache
handleInputMiss i = getFreeTrans >>= f
  where f Nothing = pure () >> outputStallReason missWithNoFreeTrans
        f (Just tid) = submitTrans tid (i ^. instAddr) >> outputStallReason normalMiss -- state change: new trans

outputStallReason :: StallReason -> ICacheM ()
outputStallReason = (cacheOutput . stallReason .=)

submitTrans :: TransId -> Addr -> ICacheM ()
submitTrans tid addr = f >>=
                           \way -> cacheState . transInfo . trans . ix (transIdx tid) .= emptyTrans { _transAddr = addr, _fillWay = way, _transState = TransAddressing }
                                >> cacheState . transInfo . priorTrans .= tid
                                >> cacheState . request .= Just addr
  where f = getLRU (getAddrSet addr)

handleTrans :: CacheState -> ICache
handleTrans cs i = genRefillOutput (cs ^. transInfo) >> handleAxiAddrReady (i ^. arready) cs >> genAxiAddrOutput (cs ^. transInfo)
                >> genAxiRReady (cs ^. transInfo) >> handleAxiData i cs
