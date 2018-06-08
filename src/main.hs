import System.Environment
import Data.Aeson

import Machine.Data
import Engine.Data
import Engine.Engine
import Logger.Logger
import Checker.Checker

import qualified Data.ByteString.Lazy as B
fget filename = B.readFile filename

usage :: IO ()
usage = do
  name <- getProgName
  putStrLn $ "usage: " ++ (show name) ++ " [-h] jsonfile input"
  putStrLn $ ""
  putStrLn $ "positional arguments:"
  putStrLn $ "  jsonfile              json description of the machine"
  putStrLn $ ""
  putStrLn $ "  input                 input of the machine"
  putStrLn $ ""
  putStrLn $ "optional arguments:"
  putStrLn $ "  -h, --help            show this help message and exit"

main :: IO ()
main = do
  args <- getArgs
  if ((length args) >= 1) && (((args !! 0) == "-h") || ((args !! 0) == "--help")) then
     usage
  else do
  if not ((length args) == 2) then
    putStrLn "missing parameters"
  else do
  let input = (args !! 1)
  s <- fget (args !! 0)
  case isValidMachine (decode s :: Maybe Machine) of
      Nothing       -> putStrLn "machine invalid"
      Just machine  ->
        case (isValidInput input (alphabet machine) (blank machine)) of
          Just input   -> run machine input
          Nothing      -> putStrLn "input invalid"
