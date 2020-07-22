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
testInputs = [ input { _instAddr = 100, _resetn = False }
             , input { _instAddr = 200, _resetn = False }
             , input { _instAddr = 1 }
             , input { _instAddr = 2 }
             , input { _instAddr = 100, _arready = True }
             ]

-- END TEST VARIABLES

-- >>> evalTestWith testInputs ^?! ix 4 . _2 . transInfo
-- TransInfo {_priorTrans = Trans0, _trans = <<Trans {_transAddr = 0000_0000_0000_0000_0000_0000_0000_0000, _transState = TransAvailable, _fillWay = Way0},Trans {_transAddr = 0000_0000_0000_0000_0000_0000_0000_0000, _transState = TransAvailable, _fillWay = Way0}>
-- Trans {_transAddr = 0000_0000_0000_0000_0000_0000_0000_0000, _transState = TransAvailable, _fillWay = Way0},Trans {_transAddr = 0000_0000_0000_0000_0000_0000_0000_0000, _transState = TransAvailable, _fillWay = Way0}>}
