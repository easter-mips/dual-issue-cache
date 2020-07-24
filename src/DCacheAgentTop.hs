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
import DCacheAgent.Agent (dcacheAgentT)
import DCacheAgent.Types

{-# ANN topEntity
  (Synthesize
    { t_name   = "dcache_agent"
    , t_inputs = [ PortName "clock"
                 , PortName "__reset"
                 , PortName "__en"
                 , PortProduct "" [ PortName "resetn"
                                  , PortName "addr_mem"
                                  , PortName "en_mem"
                                  , PortName "wen_mem"
                                  , PortName "rwsize_mem"
                                  , PortName "wstrb_mem"
                                  , PortName "wdata_mem"
                                  , PortName "wait_cache"
                                  , PortName "rdata_cache"
                                  ]
                 ]
    , t_output = PortProduct "" [ PortName "addr_cache"
                                , PortName "en_cache"
                                , PortName "wen_cache"
                                , PortName "rwsize_cache"
                                , PortName "wstrb_cache"
                                , PortName "wdata_cache"
                                , PortName "wait_mem"
                                , PortName "rdata_mem"
                                ]
    }) #-}
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System AgentInput
  -> Signal System AgentOutput
topEntity = exposeClockResetEnable agent
  where agent = mealy dcacheAgentT CacheFree
