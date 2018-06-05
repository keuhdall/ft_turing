module Checker.Checker (isValidMachine, isValidInput, canReachEnd) where
    import Prelude hiding (read)
    import Data.Map
    import Machine.Data
    import Engine.Data
    import Engine.Engine

    isValidMachine :: Maybe Machine -> Maybe Machine
    isValidMachine machine = case machine of
        Just machine    -> if (canReachEnd machine) then checkMachine machine else Nothing
        Nothing         -> Nothing
        where
            canReachEnd :: Machine -> Bool
            canReachEnd m = 
                canReachEnd' m (toList (transitions m))                    
                where
                    canReachEnd' :: Machine -> [(String, [ActionTransition])] -> Bool
                    canReachEnd' m transitionsList = case (transitionsList m) of
                        (hd:tl) ->
                            if (parseActionTransitionList (snd hd)) then
                                True
                            else
                                canReachEnd' m tl
                        []      -> False

                    parseActionTransitionList :: Machine -> [ActionTransition] -> Bool
                    parseActionTransitionList m atList = case atList of
                        (hd:tl) ->
                            if ((action hd) `member` (finals m)) then
                                True
                            else
                                parseActionTransitionList m tl
                        []      -> False

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


    isValidInput :: String -> [String] -> String -> Maybe String
    isValidInput input alphabet blank =
        if isValidInput' input alphabet blank then Just input else Nothing
        where
            isValidInput' :: String -> [String] -> String -> Bool
            isValidInput' input alphabet blank = case input of
                [] -> True
                (c:s) -> if not (isValidWord [c] alphabet blank) then False else (isValidInput' s alphabet blank)

            isValidWord :: String -> [String] -> String -> Bool
            isValidWord w alphabet blank =
                if w == blank then False else w `elem` alphabet