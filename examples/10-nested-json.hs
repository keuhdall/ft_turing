-- https://haskell-lang.org/library/aeson
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import qualified Data.Map as Map

fget filename = B.readFile filename

data Cond = Cond {read::String, to_state::String, write::String,action::String} deriving (Generic, Show)
instance FromJSON Cond
instance ToJSON Cond

data M = M {transitions::Map.Map String [Cond]} deriving (Generic, Show)
instance FromJSON M
instance ToJSON M

main = do
  args <- getArgs
  if not ((length args) == 1) then do
                              print "usage: ./bin file.json"
                              return ()
  else do
  let f = (args !! 0)
  s <- (fget f)
  let v = decode s :: Maybe M
  print v
