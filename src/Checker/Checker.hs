{-# LANGUAGE NamedFieldPuns #-}

module Checker.Checker (isValidMachine, isValidInput) where
    import Prelude hiding (read)
    import Data.Map hiding (null,filter)
    import Machine.Data
    import Engine.Data
    import Engine.Engine

    isValidMachine :: Machine -> Maybe Machine
    isValidMachine m@Machine{transitions,finals} = if canHalt then checkMachine m else Nothing where
      canHalt = not . null $ filter (\x -> (to_state x) `elem` finals) (concat $ elems transitions)


    checkMachine :: Machine -> Maybe Machine
    checkMachine machine =
        (checkTransitions machine (states machine))
        where
          checkActionTransitions :: Machine -> [ActionTransition] -> Bool
          checkActionTransitions m at = case at of
              (hd:tl) -> checkActionTransitions' m hd tl
              []      -> True
              where
                checkActionTransitions' :: Machine -> ActionTransition -> [ActionTransition] -> Bool
                checkActionTransitions' m hd tl
                    | ((((read      hd)     `elem`  (alphabet   m))   || (read  hd) == "ANY")                             &&
                        ((to_state  hd)     `elem`  (states     m))                                                       &&
                        ((write     hd)     `elem`  (alphabet   m)    || (write hd) == "ANY" || (write hd) == (blank m))  &&
                        ((action    hd) == "RIGHT" || (action hd) == "LEFT")) = checkActionTransitions m tl
                    | otherwise = False

          checkTransitions :: Machine -> [String] -> Maybe Machine
          checkTransitions m states = case states of
              (hd:tl) -> checkTransitions' m hd tl                              
              []      -> Just m
              where
                checkTransitions' :: Machine -> String -> [String] -> Maybe Machine
                checkTransitions' m hd tl
                    | (hd `elem` (finals m)) = checkTransitions m tl
                    | ((hd `member` (transitions m)) && (checkActionTransitions m ((transitions m) ! hd))) = checkTransitions m tl
                    | otherwise = Nothing

    isValidInput :: String -> Machine -> Maybe Machine
    isValidInput input m@Machine{alphabet,blank}
      | all (flip elem alphabet) pureInput && all (/=blank) pureInput = Just m
      | otherwise = Nothing where
        pureInput = pure <$> input