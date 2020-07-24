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
import ICache.Types
import ICache.Cache

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
                                , PortName "stall_reason_o"
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
