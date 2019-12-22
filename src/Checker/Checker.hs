{-# LANGUAGE NamedFieldPuns #-}

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
                        (hd:tl) -> canReachEnd'' m hd tl
                        []      -> (Nothing, "can\'t reach end!")
                        where
                            canReachEnd'' :: Machine -> (String, [ActionTransition]) -> [(String, [ActionTransition])] -> (Maybe Machine, String)
                            canReachEnd'' m hd tl
                                | (parseActionTransitionList m (snd hd)) = (checkMachine m)
                                | otherwise = canReachEnd' m tl

                    parseActionTransitionList :: Machine -> [ActionTransition] -> Bool
                    parseActionTransitionList m atList = case atList of
                        (hd:tl) -> parseActionTransitionList' m hd tl
                        []      -> False
                        where
                            parseActionTransitionList' :: Machine -> ActionTransition -> [ActionTransition] -> Bool
                            parseActionTransitionList' m hd tl
                                | ((to_state hd) `elem` (finals m)) = True
                                | otherwise = parseActionTransitionList m tl

            checkMachine :: Machine -> (Maybe Machine, String)
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

                  checkTransitions :: Machine -> [String] -> (Maybe Machine, String)
                  checkTransitions m states = case states of
                      (hd:tl) -> checkTransitions' m hd tl                              
                      []      -> (Just m, "OK")
                      where
                        checkTransitions' :: Machine -> String -> [String] -> (Maybe Machine, String)
                        checkTransitions' m hd tl
                            | (hd `elem` (finals m)) = checkTransitions m tl
                            | ((hd `member` (transitions m)) && (checkActionTransitions m ((transitions m) ! hd))) = checkTransitions m tl
                            | otherwise = (Nothing, "error at " ++ (show hd))

    isValidInput :: String -> Machine -> Maybe String
    isValidInput input Machine{alphabet,blank}
      | all (flip elem alphabet) pureInput && all (/=blank) pureInput = Just input
      | otherwise = Nothing where
        pureInput = pure <$> input