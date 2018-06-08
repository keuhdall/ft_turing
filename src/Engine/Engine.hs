module Engine.Engine (run) where
    import Prelude hiding (read)
    import Data.Map
    import Logger.Logger
    import Machine.Data
    import Engine.Data

    run :: Machine -> String -> IO ()
    run machine input = do
      let pos = 0 :: Int
      let engine = Engine 0 0 (initial machine) "RIGHT" (makeTape input)
      printHeader machine
      next engine machine
      where
        makeTape :: String -> [String]
        makeTape s = case s of
            (c:ns) -> [([c])] ++ (makeTape ns)
            [] -> []

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
                printStep engine machine t
                case (apply t engine machine) of
                    Just engine' -> next engine' machine
                    Nothing -> putStrLn "program finished"
            where
                apply :: ActionTransition -> Engine -> Machine -> Maybe Engine
                apply t engine machine =
                    if ((to_state t) `elem` (finals machine)) then
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

                replace :: [String] -> Int -> String -> String -> [String]
                replace tape pos w blank = replace' tape 0 pos w blank where
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
                                Just ts -> findTransition w ts
                    where
                        findTransition :: String -> [ActionTransition] -> Maybe ActionTransition
                        findTransition w ts = case ts of
                            [] -> Nothing
                            (t:nts) -> if (read t) == w then Just t else (findTransition w nts)
