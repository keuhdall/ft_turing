{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import GHC.Generics
import Data.Map
import Data.Aeson

data Transition = Transition {
  read      :: String,
  write     :: String,
  to_state  :: String,
  action    :: String
} deriving (Generic, Show, FromJSON, ToJSON)

data Machine = Machine {
  name        :: String,
  states      :: [String],
  alphabet    :: [String],
  blank       :: String,
  initial     :: String,
  finals      :: [String],
  transitions :: Map String [Transition]
} deriving (Generic, Show, FromJSON, ToJSON)

data Engine = Engine {
  step    :: Int,
  pos     :: Int,
  initpos :: Int,
  state   :: String,
  tape    :: [String]
} deriving (Show)