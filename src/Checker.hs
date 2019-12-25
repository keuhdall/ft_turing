{-# LANGUAGE NamedFieldPuns #-}

module Checker (isValidMachine, isValidInput) where
  import Prelude hiding (read)
  import Data.Map hiding (null,filter)
  import Types

  isValidMachine :: Machine -> Maybe Machine
  isValidMachine m@Machine{transitions,finals} = if canHalt then checkMachine m else Nothing where
    canHalt = not . null $ filter (\x -> (to_state x) `elem` finals) (concat $ elems transitions)

  checkMachine :: Machine -> Maybe Machine
  checkMachine m@Machine{alphabet,blank,states,finals,transitions} = if all checkState states then Just m else Nothing where
    checkState st = st `elem` finals || (st `member` transitions && all isValidTransition transitionList)
    isValidTransition ActionTransition{read,to_state,write,action} =
      (read `elem` alphabet || read == "ANY") &&
      to_state `elem` states &&
      (write `elem` alphabet || write == "ANY" || write == blank) &&
      (action == "RIGHT" || action == "LEFT")
    transitionList = concat $ elems transitions

  isValidInput :: String -> Machine -> Maybe Machine
  isValidInput input m@Machine{alphabet,blank}
    | all (flip elem alphabet) pureInput && all (/=blank) pureInput = Just m
    | otherwise = Nothing where
      pureInput = pure <$> input