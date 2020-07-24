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
module DCacheAgent.Types where
import Clash.Prelude

data AgentState = CacheFree | CacheBusy AgentInput
                deriving (Eq, Show, Generic, NFDataX)

type Addr = BitVector 32
type Data = BitVector 32
type MemSize = BitVector 3
type MemStrb = BitVector 4

data AgentInput = AgentInput { resetn :: Bool
                             , memAddr :: Addr
                             , memEnable :: Bool
                             , memWriteEnable :: Bool
                             , memSize :: MemSize
                             , memStrb :: MemStrb
                             , memWriteData :: Data

                             , dcWait :: Bool
                             , dcReadData :: Data
                             } deriving (Eq, Show, Generic, NFDataX)

data AgentOutput = AgentOutput { dcAddr :: Addr
                               , dcEnable :: Bool
                               , dcWriteEnable :: Bool
                               , dcSize :: MemSize
                               , dcStrb :: MemStrb
                               , dcWriteData :: Data

                               , memWait :: Bool
                               , memReadData :: Data
                               } deriving (Eq, Show, Generic, NFDataX)
