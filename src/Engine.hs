{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Engine (run) where

import Prelude hiding (read, lookup)
import Data.Map hiding (map, filter, null, (!?))
import Data.List.Split (chunksOf)

import Types
import Logger

initialState :: Machine -> String -> Engine
initialState Machine{initial} s = Engine {
  step    = 0,
  pos     = 0,
  initpos = 0,
  state   = initial,
  tape    = chunksOf 1 s
}

(!?) :: [a] -> Int -> Maybe a
(!?) a b = if b < 0 || b >= length a then Nothing else Just $ a !! b

currentWord :: Engine -> Maybe String
currentWord Engine{pos,tape} = if pos < 0 then Nothing else tape !? pos

run :: Machine -> String -> IO ()
run m@Machine{..} input = do
  printHeader m
  next $ initialState m input
  where
    next :: Engine -> IO ()
    next e = case currentWord e >>= extractTransition e >>= apply e of
      Just (e', t) -> do
        printStep e m t
        next e'
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
        nextPos = if action == "RIGHT" then 1 else -1
        newPos = pos + nextPos

    replace :: Engine -> String -> Int -> [String]
    replace Engine{tape,pos} w newpos = checkTape <$> zip filledTape [0..] where
      checkTape (a,b) = if b == pos then w else a
      filledTape = let x = abs $ newpos + 1 in if newpos < 0 then ([0..x] >> blank):tape else tape

    extractTransition :: Engine -> String -> Maybe Transition
    extractTransition Engine{state} w = t >>= \t' -> findTransition (-1) $ zip t' [0..] where
      t = lookup state transitions
      findTransition :: Int -> [(Transition, Int)] -> Maybe Transition
      findTransition pos t' = case t' of
        ((a,b):xs)  -> if read a == w then Just a else findTransition (if pos == -1 && read a == "ANY" then b else pos) xs
        []          -> if pos == -1 then Nothing else t >>= buildTransition w pos
      buildTransition :: String -> Int -> [Transition] -> Maybe Transition
      buildTransition s n t = Just Transition {
        read      = s,
        write     = write $ t !! n,
        to_state  = to_state $ t !! n,
        action    = action $ t !! n
      }
