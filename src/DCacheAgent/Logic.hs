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
module DCacheAgent.Logic where
import Clash.Prelude
import DCacheAgent.Types

genReadData :: AgentInput -> AgentState -> Data
genReadData AgentInput{..} _ = dcReadData

genWait :: AgentInput -> AgentState -> Bool
genWait AgentInput{..} (CacheBusy _) = memEnable
genWait AgentInput{..} CacheFree = f $ needsGuard memEnable memWriteEnable dcWait
  where f True = True
        f False = dcWait

genMemReq :: AgentInput -> AgentState -> (Addr, Bool, Bool, MemSize, MemStrb, Data)
genMemReq AgentInput{..} CacheFree = ( memAddr, memEnable, memWriteEnable
                                     , memSize, memStrb, memWriteData
                                     )
genMemReq _ (CacheBusy AgentInput{..}) = ( memAddr, memEnable, memWriteEnable
                                         , memSize, memStrb, memWriteData
                                         )

nextState :: AgentInput -> AgentState -> AgentState
nextState i@AgentInput{..} CacheFree = f $ needsGuard memEnable memWriteEnable dcWait
  where f True = CacheBusy i
        f False = CacheFree
nextState AgentInput{..} s@(CacheBusy _) | not dcWait = CacheFree
                                         | otherwise = s

needsGuard :: Bool -- mem enable
           -> Bool -- mem write enable
           -> Bool -- cache wait
           -> Bool
needsGuard x y z = x && y && z
