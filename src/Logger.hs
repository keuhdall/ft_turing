{-# LANGUAGE NamedFieldPuns #-}

module Logger (printHeader, printStep, printUsage) where

import Prelude hiding (read)
import Data.List (intercalate)
import Control.Monad (zipWithM_)
import System.Environment (getProgName)

import Types

tapeSize :: Int
tapeSize = 30

--TODO: use printf
printHeader :: Machine -> IO ()
printHeader Machine{name,alphabet,states,initial,finals} = do
  putStrLn "********************************************************************************\n\
  \*                                                                              *"
  mapM_ printName [0..80]
  putStrLn $ "*                                                                              *\n\
  \********************************************************************************\n\
  \Alphabet: "  ++ "[" ++ intercalate ", " alphabet ++ "]\n\
  \States: "    ++ "[" ++ intercalate ", " states   ++ "]\n\
  \Initial: "   ++ initial ++ "\n\
  \Finals: "    ++ "[" ++ intercalate ", " finals   ++ "]\n\
  \********************************************************************************"
  where
    printName n
      | n == trailForName `quot` 2  = putStr name
      | n == 0 || n == trailForName = putStr "*"
      | n == 80   = putStr "\n"
      | otherwise = putStr " "
      where
        trailForName = 80 - length name

trail :: Int -> String -> String
trail n s = if length s < n then s ++ ([0..n-length s] >> " ") else s

printStep :: Engine -> Machine -> ActionTransition -> IO ()
printStep Engine{step,pos,initpos,state,tape} Machine{blank} ActionTransition{read,write,to_state,action} = do
  (putStr . trail 3 . show) step >> putStr "[" >> zipWithM_ printTape (adjustTape tape) [0..tapeSize] >> putStr "]"
  putStr $ " (\'" ++ read ++ "\', "++ state ++") -> ( \'" ++ write ++ "\', "++ to_state ++ ", "++ action ++")\n"
  where
    adjustTape :: [String] -> [String]
    adjustTape xs = let size = length xs in if size < tapeSize then xs ++ ([0..tapeSize-size] >> [blank]) else xs

    printTape :: String -> Int -> IO ()
    printTape s n
      | n == pos = putStr $ "\x1b[1;32m\x1b[41m" ++ s ++ "\x1b[0m"
      | n == initpos && n /= 0 = putStr $ "\x1b[44m" ++ s ++ "\x1b[0m"
      | otherwise = putStr s

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn $ "usage: " ++ name ++ " [-h] jsonfile input\
  \\n\
  \positional arguments:\n\
  \jsonfile              json description of the machine\n\
  \\n\
  \input                 input of the machine\n\
  \\n\
  \optional arguments:\n\
  \-h, --help            show this help message and exit"
