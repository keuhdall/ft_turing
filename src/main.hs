import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.Aeson

import Machine.Data
import Engine.Data
import Engine.Engine
import Logger.Logger
import Checker.Checker

import qualified Data.ByteString.Lazy as B

fget = B.readFile

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
  s <- fget $ args !! 0
  let input = args !! 1
  case isValidMachine (decode s :: Maybe Machine) of
      (Nothing, nok)       -> putStrLn $ "machine invalid: " ++ nok
      (Just machine, ok) ->
        case (isValidInput input machine) of
          Just input   -> run machine input
          Nothing      -> putStrLn "input invalid"
