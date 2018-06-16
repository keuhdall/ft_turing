import System.Environment
import Data.Aeson

import Machine.Data
import Engine.Data
import Engine.Engine
import Logger.Logger
import Checker.Checker

import qualified Data.ByteString.Lazy as B
fget filename = B.readFile filename

main :: IO ()
main = do
  args <- getArgs
  if ((length args) >= 1) && (((args !! 0) == "-h") || ((args !! 0) == "--help")) then
     printUsage
  else do
  if not ((length args) == 2) then
    putStrLn "missing parameters"
  else do
  let input = (args !! 1)
  if (length input) == 0 then
    putStrLn $ "empty input"
  else do
  s <- fget (args !! 0)
  case isValidMachine (decode s :: Maybe Machine) of
      (Nothing, nok)       -> putStrLn $ "machine invalid: " ++ nok
      (Just machine, ok) ->
        case (isValidInput input (alphabet machine) (blank machine)) of
          Just input   -> run machine input
          Nothing      -> putStrLn "input invalid"
