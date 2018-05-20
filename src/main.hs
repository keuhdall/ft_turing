{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (read)
import System.Environment
import qualified Data.Map as Map

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
  transitions::Map.Map String [Cond]
} deriving (Generic, Show)
instance FromJSON Machine
instance ToJSON Machine

valid machine = machine --TODO: check the machine is valid else return Nothing

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
  (s:ls) -> s ++ (formatTape ls)
  [] -> ""

printStep engine t = do
  putStr $ "["++(formatTape (tape engine))++"]" ++ "\n"
  putStr $ "Debug step "++(show (step engine))++"\n"
  print engine

findTransition w ts = case ts of
  (t:nts) -> if (read t) == w then t else (findTransition w nts)
--[] ->

extractTransition machine s w =
  case (Map.lookup s (transitions machine)) of
    Just ts -> findTransition w ts
--Nothing ->

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

run machine input = do
  let pos = 0 :: Int
  let engine = Engine 0 0 (initial machine) "RIGHT" (makeTape input)
  putStr "start\n"
  next engine machine
  return (putStr "finish\n")

-- main
main = do
  args <- getArgs
  let input = (args !! 1)
  putStr "parsing the json...\n"
  s <- fget (args !! 0)
  case (decode s :: Maybe Machine) of
      Just machine -> run machine input
      Nothing -> return (putStr "failed to open input\n")
