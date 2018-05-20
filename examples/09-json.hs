-- https://haskell-lang.org/library/aeson
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

fget filename = B.readFile filename

data People = People {firstName::String, lastName::String, age::Int, likesPizza::Bool} deriving (Generic, Show)
instance FromJSON People
instance ToJSON People

main = do
  args <- getArgs
  if not ((length args) == 1) then do
                              print "usage: ./bin file.json"
                              return ()
  else do
  let f = (args !! 0)
  s <- (fget f)
  let v = decode s :: Maybe [People]
  print v
