module Logger.Logger (printLine, printHeader, printStep) where
    import Prelude hiding (read,write)
    import Machine.Data
    import Engine.Data

    trail :: String -> String -> Int -> String
    trail s c i = if (length s) < i then trail (s ++ c) c i else s

    printLine :: IO ()
    printLine = printLine' 0 where
        printLine' :: Int -> IO ()
        printLine' n =
            if (n < 80) then do
                putStr "*"
                printLine' $ n + 1
            else
                putStr "\n"

    getList' :: [String] -> String
    getList' l = case l of
        (s:[]) -> s
        (s:l') -> s ++ ", " ++ (getList' l')
        [] -> ""

    getList :: [String] -> String
    getList l = "[ " ++ (getList' l) ++ " ]"

    printHeader :: Machine -> IO ()
    printHeader machine = do
        printLine
        printEmptyLine 0
        printName (name machine) 0
        printEmptyLine 0
        printLine
        putStrLn $ "Alphabet: " ++ (getList (alphabet machine))
        putStrLn $ "States: " ++ (getList (states machine))
        putStrLn $ "Initial: " ++ (initial machine)
        putStrLn $ "Finals: " ++ (getList (finals machine))
        printLine
        where
            printEmptyLine :: Int -> IO ()
            printEmptyLine n =
                if (n < 80) then do
                    if (n == 0 || n == 79) then
                        putStr "*"
                    else
                        putStr " "
                    printEmptyLine $ n + 1
                else
                    putStr "\n"

            printName :: String -> Int -> IO ()
            printName name n =
                if (n == ((80 - length name) `quot` 2)) then do
                    putStr name
                    printName name $ n + 1
                else if (n < 80) then do
                    if (n == 0 || n == (80 - length name)) then
                        putStr "*"
                    else
                        putStr " "
                    printName name $ n + 1
                else
                    putStr "\n"

    printStep :: Engine -> Machine -> ActionTransition -> IO ()
    printStep engine machine t = do
        putStrLn $ (trail (show (step engine)) " " 3) ++ "["++(formatTape (tape engine) (pos engine) 0 30 (blank machine))++"]" ++ " (\'" ++ (read t) ++ "\', "++ (state engine) ++") -> ( \'" ++ (write t) ++ "\', "++ (to_state t) ++ ", "++ (action t) ++")"
        where
            formatTape :: [String] -> Int -> Int -> Int -> String -> String
            formatTape tape pos i n blank = case tape of
                (s:ls)  -> (if i == pos then "\x1b[1;32m\x1b[41m" ++ s ++ "\x1b[0m" else s) ++ (formatTape ls pos (i+1) n blank)
                []      -> if i < n then blank ++ (formatTape tape pos (i + 1) n blank) else ""
