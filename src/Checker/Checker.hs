module Checker.Checker (isValidMachine, isValidInput) where
    import Prelude hiding (read)
    import Data.Map
    import Machine.Data
    import Engine.Data
    import Engine.Engine

    isValidMachine :: Maybe Machine -> (Maybe Machine, String)
    isValidMachine machine = case machine of
        Just machine    -> canReachEnd machine
        Nothing         -> (Nothing, "No machine provided")
        where
            canReachEnd :: Machine -> (Maybe Machine, String)
            canReachEnd m =
                canReachEnd' m (toList (transitions m))
                where
                    canReachEnd' :: Machine -> [(String, [ActionTransition])] -> (Maybe Machine, String)
                    canReachEnd' m transitionsList = case transitionsList of
                        (hd:tl) ->
                            if (parseActionTransitionList m (snd hd)) then
                                (checkMachine m)
                            else
                                canReachEnd' m tl
                        []      -> (Nothing, "can\'t reach end!")

                    parseActionTransitionList :: Machine -> [ActionTransition] -> Bool
                    parseActionTransitionList m atList = case atList of
                        (hd:tl) ->
                            if ((to_state hd) `elem` (finals m)) then
                                True
                            else
                                parseActionTransitionList m tl
                        []      -> False

            checkMachine :: Machine -> (Maybe Machine, String)
            checkMachine machine =
                (checkTransitions machine (states machine))
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

                  checkTransitions :: Machine -> [String] -> (Maybe Machine, String)
                  checkTransitions machine states = case states of
                      (hd:tl) ->
                          if (hd `elem` (finals machine)) then
                              checkTransitions machine tl
                          else if ((hd `member` (transitions machine)) && (checkActionTransitions machine ((transitions machine) ! hd))) then
                              checkTransitions machine tl
                          else
                              (Nothing, "error at " ++ (show hd))
                      []      -> (Just machine, "OK")

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
