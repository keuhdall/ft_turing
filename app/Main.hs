module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.Aeson

import Types
import Checker
import Logger
import Engine

import qualified Data.ByteString.Lazy as B


checkArgs :: [String] -> IO [String]
checkArgs xs
  | length xs < 2 || first `elem` ["-h","--help"] = printUsage >> exitSuccess
  | null second = putStrLn "Error: empty input" >> exitSuccess
  | otherwise = pure xs where
    first   = xs !! 0
    second  = xs !! 1

main :: IO ()
main = do
  args <- checkArgs =<< getArgs
  s <- B.readFile $ args !! 0
  let input = args !! 1
  case (decode s :: Maybe Machine) >>= isValidMachine >>= isValidInput input of
    Nothing -> putStrLn "Error : please check machine or input"
    Just m  -> run m input