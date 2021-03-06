{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Engine (run) where

import Prelude hiding (read, lookup)
import Data.Map hiding (map, filter, null, (!?))
import Data.List.Split (chunksOf)

import Types
import Logger

setInitialState :: Machine -> String -> Engine
setInitialState Machine{initial} s = Engine {
  step    = 0,
  pos     = 0,
  initpos = 0,
  state   = initial,
  tape    = chunksOf 1 s
}

(!?) :: [a] -> Int -> Maybe a
(!?) a b = if b < 0 || b >= length a then Nothing else Just $ a !! b

currentWord :: Engine -> Maybe String
currentWord Engine{pos,tape} = tape !? pos

displayInitialState :: Engine -> Machine -> IO ()
displayInitialState e m = case currentWord e >>= extractTransition e m of
  Just t  -> printStep e m t
  _       -> pure ()

extractTransition :: Engine -> Machine -> String -> Maybe Transition
extractTransition Engine{state} Machine{transitions} w = t >>= \t' -> findTransition (-1) $ zip t' [0..] where
  t = lookup state transitions
  findTransition :: Int -> [(Transition, Int)] -> Maybe Transition
  findTransition pos t' = case t' of
    (a,b):xs  -> if read a == w then Just a else findTransition (if pos == -1 && read a == "ANY" then b else pos) xs
    []        -> if pos == -1 then Nothing else t >>= \t'' -> Just (t'' !! pos) {read = w}

run :: Machine -> String -> IO ()
run m@Machine{..} input = do
  let initState = setInitialState m input
  printHeader m
  displayInitialState initState m
  next initState
  where
    next :: Engine -> IO ()
    next e = case currentWord e >>= extractTransition e m >>= apply e of
      Just (e', t) -> printStep e' m t >> next e'
      Nothing -> putStrLn "program finished"

    apply :: Engine -> Transition -> Maybe (Engine,Transition)
    apply e@Engine{step,pos,initpos} t@Transition{..}
      | to_state `elem` finals  = Nothing
      | otherwise = Just (Engine {
        step    = step + 1,
        pos     = if newPos < 0 then 0 else newPos,
        initpos = if newPos < 0 then initpos + 1 else initpos,
        state   = to_state,
        tape    = replace e (if write == "ANY" then read else write) newPos
      }, t) where
        newPos = pos + if action == "RIGHT" then 1 else -1

    replace :: Engine -> String -> Int -> [String]
    replace Engine{tape,pos} w newpos = checkTape <$> zip filledTape [0..] where
      checkTape (a,b) = if b == pos then w else a
      filledTape = let x = abs $ newpos + 1 in if newpos < 0 then ([0..x] >> blank):tape else tape