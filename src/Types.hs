{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Map
import Data.Aeson

data ActionTransition = ActionTransition {
  read      :: String,
  to_state  :: String,
  write     :: String,
  action    :: String
} deriving (Generic, Show, FromJSON, ToJSON)

data Machine = Machine {
  name      :: String,
  states    :: [String],
  alphabet  :: [String],
  blank     :: String,
  initial   :: String,
  finals    :: [String],
  transitions :: Map String [ActionTransition]
} deriving (Generic, Show, FromJSON, ToJSON)

data Engine = Engine {
  step    :: Int,
  pos     :: Int,
  initpos :: Int,
  state   :: String,
  tape    :: [String]
} deriving (Show)