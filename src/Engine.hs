{-# LANGUAGE NamedFieldPuns #-}

module Engine (run) where

import Prelude hiding (read, lookup)
import Data.Map hiding (map, filter, null)
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

currentWord :: Engine -> Maybe String
currentWord Engine{pos,tape} = if pos < 0 then Nothing else Just $ tape !! pos

run :: Machine -> String -> IO ()
run m@Machine{blank,finals,transitions} input = do
  printHeader m
  next $ initialState m input
  where
    next :: Engine -> IO ()
    next e = case (currentWord e >>= extractTransition e >>= apply e) of
      Just (e', t) -> do
        printStep e m t
        next e'
      Nothing -> putStrLn "program finished"

    apply :: Engine -> ActionTransition -> Maybe (Engine,ActionTransition)
    apply e@Engine{step,pos,initpos} t@ActionTransition{read,write,to_state,action}
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

    extractTransition :: Engine -> String -> Maybe ActionTransition
    extractTransition Engine{state} w = case (lookup state transitions) of
      Just ts -> findTransition w ts 0 (-1) ts
      Nothing -> Nothing
      where
        findTransition :: String -> [ActionTransition] -> Int -> Int -> [ActionTransition] -> Maybe ActionTransition
        findTransition w' ts i posAny x = case ts of
          (t:nts) ->
            if read t == w' then Just t
            else findTransition w' nts (i+1) (if posAny == -1 && (read t) == "ANY" then i else posAny) x
          []      ->
            if posAny == -1 then Nothing
            else Just $ buildTransition w' (x !! posAny) where
            buildTransition :: String -> ActionTransition -> ActionTransition
            buildTransition s ActionTransition{to_state,write,action} = ActionTransition s to_state write action