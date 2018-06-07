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
  let input = (args !! 1)
  putStrLn "parsing the json..."
  s <- fget (args !! 0)
  case isValidMachine (decode s :: Maybe Machine) of
      Nothing       -> putStrLn "machine invalid"
      Just machine  ->
        case (isValidInput input (alphabet machine) (blank machine)) of
          Just input   -> run machine input
          Nothing      -> putStrLn "input invalid"