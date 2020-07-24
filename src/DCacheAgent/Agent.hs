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
module DCacheAgent.Agent where
import Clash.Prelude
import DCacheAgent.Types
import DCacheAgent.Logic

dcacheAgentT :: AgentState -> AgentInput -> (AgentState, AgentOutput)
dcacheAgentT s i@AgentInput{..}
  | resetn = (s', AgentOutput{..})
  | otherwise = (CacheFree, AgentOutput 0 False False 0 0 0 False 0)
  where s' = nextState i s
        memReadData = genReadData i s
        memWait = genWait i s
        (  dcAddr, dcEnable, dcWriteEnable
         , dcSize, dcStrb, dcWriteData
         ) = genMemReq i s
