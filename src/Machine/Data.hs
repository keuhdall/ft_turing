{-# LANGUAGE DeriveGeneric #-}

module Machine.Data (ActionTransition(..), Machine(..)) where
    import qualified Data.ByteString.Lazy as B    
    import GHC.Generics
    import Data.Map
    import Data.Aeson

    data ActionTransition = ActionTransition {
        read::String,
        to_state::String,
        write::String,
        action::String
    } deriving (Generic, Show)
    instance FromJSON ActionTransition
    instance ToJSON ActionTransition

    data Machine = Machine {
        name::String,
        alphabet::[String],
        blank::String,
        states::[String],
        initial::String,
        finals::[String],
        transitions::Map String [ActionTransition]
    } deriving (Generic, Show)
    instance FromJSON Machine
    instance ToJSON Machine
      