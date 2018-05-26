{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (read)
import System.Environment
import Data.Map

-- Json
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

fget filename = B.readFile filename

-- Turing
data Cond = Cond {
  read::String,
  to_state::String,
  write::String,
  action::String
} deriving (Generic, Show)
instance FromJSON Cond
instance ToJSON Cond

data Machine = Machine {
  name::String,
  alphabet::[String],
  blank::String,
  states::[String],
  initial::String,
  finals::[String],
  transitions::Map String [Cond]
} deriving (Generic, Show)
instance FromJSON Machine
instance ToJSON Machine

doCheck machine = Just machine --TODO: check the machine is valid else return Nothing

valid m = case m of
  Just machine -> doCheck machine
  Nothing -> Nothing

checkMachine :: Machine -> Maybe Machine
checkMachine machine =
    if (checkTransitions machine (alphabet machine)) then
        Just machine
    else
        Nothing
    where
      checkActionTransitions :: Machine -> [Cond] -> Bool
      checkActionTransitions m at = case at of
          (hd:tl) ->
              if (((read hd) `elem` (alphabet m)) &&
                  ((to_state hd) `elem` (states m)) &&
                  (((write hd) `elem` (alphabet m)) || (write hd) == (blank m)) &&
                  ((action hd) == "RIGHT" || (action hd) == "LEFT")) then
                      checkActionTransitions m tl
              else
                  False
          []      -> True
    
      checkTransitions :: Machine -> [String] -> Bool
      checkTransitions machine alphabet = case alphabet of
          (hd:tl) ->
              if ((hd `member` (transitions machine)) && (checkActionTransitions machine ((transitions machine) ! hd))) then
                  checkTransitions machine tl
              else
                  False
          []      -> True

isValidMachine :: Maybe Machine -> Maybe Machine
isValidMachine machine = case machine of
    Just machine    -> checkMachine machine
    Nothing         -> Nothing


-- Tape

replace' tape i pos w =
  case tape of
    (c:s) -> (if i == pos then w else c):(replace' s (i+1) pos w)
    [] -> []
replace tape pos w = replace' tape 0 pos w

makeTape :: String -> [String]
makeTape s = case s of
	(c:ns) -> [([c])] ++ (makeTape ns)
	[] -> []

-- Engine

data Engine = Engine {
	step::Int,
	pos::Int,
	state::String,
	eaction::String,
	tape::[String]
} deriving (Show)

formatTape tape = case tape of
  (s:ls)  -> s ++ (formatTape ls)
  []      -> ""

printStep engine t = do
  putStr $ "["++(formatTape (tape engine))++"]" ++ "\n"
  putStr $ "Debug step "++(show (step engine))++"\n"
  print engine

findTransition w ts = case ts of
  (t:nts) -> if (read t) == w then t else (findTransition w nts)

extractTransition machine s w =
  case (Data.Map.lookup s (transitions machine)) of
    Just ts -> findTransition w ts

currentWord engine = (tape engine) !! (pos engine)

apply t engine machine =
  if (to_state t) `elem` (finals machine) then
    Nothing
  else
    Just (
	Engine
		((step engine) + 1)
		( (pos engine) + (if (action t) == "RIGHT" then (1) else -1) )
		(to_state t)
		(action t)
		(replace (tape engine) (pos engine) (write t) )
	)

next engine machine = do
  let w = currentWord engine
  let s = state engine
  --putStr $ "will try to find " ++ (w) ++ " in " ++ (show (transitions machine))
  let t = extractTransition machine s w
  if (step engine) >= 30 then putStr("debug stop")
  else do
  printStep engine t
  case (apply t engine machine) of
    Just engine' -> next engine' machine
    Nothing -> putStr("program finished")

run :: Machine -> String -> IO ()
run machine input = do
  let pos = 0 :: Int
  let engine = Engine 0 0 (initial machine) "RIGHT" (makeTape input)
  putStr "start\n"
  next engine machine
  putStr "finish\n"

-- main
main = do
  args <- getArgs
  let input = (args !! 1)
  putStr "parsing the json...\n"
  s <- fget (args !! 0)
  case isValidMachine (decode s :: Maybe Machine) of
      Just machine  -> run machine input
      Nothing       -> putStrLn "failed to open input"
