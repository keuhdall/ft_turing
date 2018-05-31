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
data ActionTransition = ActionTransition {
  read::String,
  to_state::String,
  write::String,
  action::String
} deriving (Generic, Show)
instance FromJSON ActionTransition
instance ToJSON ActionTransition

data Machine = Machine {
  name::String,
  alphabet::[String],
  blank::String,
  states::[String],
  initial::String,
  finals::[String],
  transitions::Map String [ActionTransition]
} deriving (Generic, Show)
instance FromJSON Machine
instance ToJSON Machine

checkMachine :: Machine -> Maybe Machine
checkMachine machine =
    if (checkTransitions machine (states machine)) then
        Just machine
    else
        Nothing
    where
      checkActionTransitions :: Machine -> [ActionTransition] -> Bool
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
      checkTransitions machine states = case states of
          (hd:tl) ->
              if (hd `elem` (finals machine)) then
                  checkTransitions machine tl
              else if ((hd `member` (transitions machine)) && (checkActionTransitions machine ((transitions machine) ! hd))) then
                  checkTransitions machine tl
              else
                  False
          []      -> True

isValidMachine :: Maybe Machine -> Maybe Machine
isValidMachine machine = case machine of
    Just machine    -> checkMachine machine
    Nothing         -> Nothing

-- Tape

replace' tape i pos w blank =
  case tape of
    (c:s) -> (if i == pos then w else c):(replace' s (i+1) pos w blank)
    [] -> if pos == i-1 then [blank] else []

replace tape pos w blank = replace' tape 0 pos w blank

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

formatTape :: [String] -> String
formatTape tape = case tape of
  (s:ls)  -> s ++ (formatTape ls)
  []      -> ""

printStep :: Engine -> ActionTransition -> IO ()
printStep engine t = do
  putStrLn $ "["++(formatTape (tape engine))++"]" ++ ""
  putStrLn $ "Debug step "++(show (step engine))++""
  print engine

findTransition :: String -> [ActionTransition] -> Maybe ActionTransition
findTransition w ts = case ts of
  [] -> Nothing
  (t:nts) -> if (read t) == w then Just t else (findTransition w nts)

extractTransition :: Machine -> String -> Maybe String -> Maybe ActionTransition
extractTransition machine s w =
  case w of
  Nothing -> Nothing
  Just w ->
    case (Data.Map.lookup s (transitions machine)) of
      Just ts -> findTransition w ts

currentWord :: Engine -> Maybe String
currentWord engine = if (pos engine) < 0 then Nothing else Just ( (tape engine) !! (pos engine) )

apply :: ActionTransition -> Engine -> Machine -> Maybe Engine
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
		(replace (tape engine) (pos engine) (write t) (blank machine))
	)

next :: Engine -> Machine -> IO ()
next engine machine = do
  let w = currentWord engine
  let s = state engine
  --putStrLn $ "will try to find " ++ (w) ++ " in " ++ (show (transitions machine))
  let t = extractTransition machine s w
  case t of
    Nothing -> putStrLn "error, leaving"
    Just t -> do
      if (step engine) >= 10 then putStrLn "debug stop"
      else do
      printStep engine t
      case (apply t engine machine) of
        Just engine' -> next engine' machine
        Nothing -> putStrLn "program finished"

run :: Machine -> String -> IO ()
run machine input = do
  let pos = 0 :: Int
  let engine = Engine 0 0 (initial machine) "RIGHT" (makeTape input)
  putStrLn "start"
  next engine machine
  putStrLn "finish"

isValidWord w alphabet blank =
  if w == blank then False else w `elem` alphabet

isValidInput' input alphabet blank = case input of
  [] -> True
  (c:s) -> if not (isValidWord [c] alphabet blank) then False else (isValidInput' s alphabet blank)

isValidInput input alphabet blank =
    if isValidInput' input alphabet blank then Just input else Nothing

-- main
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
