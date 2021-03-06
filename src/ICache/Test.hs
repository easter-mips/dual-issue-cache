module ICache.Test where
import Clash.Prelude
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.RWS.Class (get, put, tell)
import Control.Lens
import ICache.Types
import ICache.Cache

type CacheTest = RWS () CRecord CState

type CRecord = [(CacheOutput, CacheState)]
type CState = CacheState

-- wrap icache into a RWS Monad
icacheTS :: CacheInput -> CacheTest CacheOutput
icacheTS i = flip icacheT i <$> get >>= f
  where f :: (CacheState, CacheOutput) -> CacheTest CacheOutput
        f (cs, co) = put cs >> tell [(co, cs)] >> return co

testWith :: [CacheInput] -> CacheTest ()
testWith = mapM_ icacheTS

evalTestWith :: [CacheInput] -> CRecord
evalTestWith i = view _3 $ runRWS (testWith i) () emptyCacheState

-- BEGIN TEST VARIABLES

emptyBank :: BankData
emptyBank = replicate d2 $ replicate d8 (0 :: Inst)

input :: CacheInput
input = CacheInput 0 True False 0 0 False False emptyBank

testInputs :: [CacheInput]
testInputs = [ input { _instAddr = 100, _resetn = False } -- 0
             , input { _instAddr = 200, _resetn = False }
             , input { _instAddr = 1 }
             , input { _instAddr = 2 }
             , input { _instAddr = 100, _arready = True }
             , input, input
             , input { _instAddr = 100, _rvalid = True, _rdata = 101, _rlast = False }
             , input { _instAddr = 100, _rvalid = True, _rdata = 102, _rlast = False }
             , input { _instAddr = 200, _rvalid = True, _rdata = 101, _rlast = False } -- 9
             , input { _instAddr = 200, _rvalid = True, _rdata = 101, _rlast = False } -- 10
             , input { _instAddr = 200, _rvalid = True, _rdata = 101, _rlast = False } -- 11
             , input { _arready = True, _instAddr = 200, _rvalid = True, _rdata = 101, _rlast = False } -- 12
             , input { _instAddr = 200, _rid = 1, _rvalid = True, _rdata = 101, _rlast = False } -- 13
             , input { _instAddr = 200, _rid = 1, _rvalid = True, _rdata = 101, _rlast = False } -- 14
             , input { _instAddr = 200, _rvalid = True, _rdata = 101, _rlast = False } -- 15
             , input { _instAddr = 208, _rid = 1, _rvalid = True, _rdata = 101, _rlast = False } -- 16
             , input { _instAddr = 208, _rid = 1, _rvalid = True, _rdata = 101, _rlast = False } -- 17
             , input { _instAddr = 208, _rid = 1, _rvalid = True, _rdata = 101, _rlast = False } -- 18
             , input { _instAddr = 200, _rid = 1, _rvalid = True, _rdata = 101, _rlast = False } -- 19
             , input { _instAddr = 200, _rid = 1, _rvalid = True, _rdata = 101, _rlast = False } -- 20
             , input { _instAddr = 200, _rid = 1, _rvalid = True, _rdata = 101, _rlast = True } -- 21
             , input { _instAddr = 200, _rid = 0, _rvalid = True, _rdata = 101, _rlast = True } -- 22
             , input { _instAddr = 204 } -- 23
             , input { _instAddr = 100 } -- 24
             , input { _instAddr = 104 } -- 25
             ]

-- END TEST VARIABLES

-- >>> evalTestWith testInputs ^?! ix 25 . _1

-- >>> getAddrSet 104
