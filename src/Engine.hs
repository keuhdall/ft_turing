module Engine (run) where

import Prelude hiding (read)
import Data.Map hiding (map)

import Types
import Logger

run :: Machine -> String -> IO ()
run m input = do
  let pos = 0 :: Int
  let engine = Engine 0 0 0 (initial m) (makeTape input)
  printHeader m
  next engine m
  where
    makeTape :: String -> [String]
    makeTape s = map (\c -> [c]) s

    next :: Engine -> Machine -> IO ()
    next engine machine = do
        let w = currentWord engine
        let s = state engine
        let t = extractTransition machine s w
        --if (step engine) >= 900 then putStrLn "stop" else do
        case t of
            Nothing -> putStrLn "error, leaving"
            Just t -> do
              printStep engine machine t
              case (apply t engine machine) of
                  Just engine' -> next engine' machine
                  Nothing -> putStrLn "program finished"
        where
            apply :: ActionTransition -> Engine -> Machine -> Maybe Engine
            apply t engine machine
                | ((to_state t) `elem` (finals machine)) = Nothing
                | otherwise = Just (
                    Engine
                        ((step engine) + 1)
                        ( case ((pos engine) + (if (action t) == "RIGHT" then (1) else -1)) of x -> if x < 0 then 0 else x )
                        ( case ((pos engine) + (if (action t) == "RIGHT" then (1) else -1)) of x -> if x < 0 then (initpos engine) + 1 else (initpos engine) )
                        (to_state t)
                        (replace (tape engine) (pos engine) (if (write t) == "ANY" then (read t) else (write t)) (blank machine) ((pos engine) + (if (action t) == "RIGHT" then (1) else -1)))
                )

            replace :: [String] -> Int -> String -> String -> Int -> [String]
            replace tape pos w blank npos
                | (npos < 0)    = replace' (blank:tape) 0   (pos + 1)   w blank
                | otherwise     = replace' tape         0   pos         w blank
                where
                replace' :: [String] -> Int -> Int -> String -> String -> [String]
                replace' tape i pos w blank =
                    case tape of
                        (c:s) -> (if i == pos then w else c):(replace' s (i+1) pos w blank)
                        [] -> if pos == i-1 then [blank] else []

            currentWord :: Engine -> Maybe String
            currentWord engine = if (pos engine) < 0 then Nothing else Just ( (tape engine) !! (pos engine) )

            -- Might be moved to Machine.Machine
            extractTransition :: Machine -> String -> Maybe String -> Maybe ActionTransition
            extractTransition machine s w =
                case w of
                    Nothing -> Nothing
                    Just w ->
                        case (Data.Map.lookup s (transitions machine)) of
                            Just ts -> findTransition w ts 0 (-1) ts
                            Nothing -> Nothing
                where
                    findTransition :: String -> [ActionTransition] -> Int -> Int -> [ActionTransition] -> Maybe ActionTransition
                    findTransition w ts i posAny x = case ts of
                        (t:nts) -> if (read t) == w then Just t else (findTransition w nts (i+1) (if ( (posAny == -1) && ((read t) == "ANY") ) then i else posAny) x )
                        []      -> if (posAny == (-1)) then Nothing else Just ( anyTransition w (x !! posAny) ) where
            anyTransition :: String -> ActionTransition -> ActionTransition
            anyTransition w t = ActionTransition w (to_state t) (write t) (action t)
