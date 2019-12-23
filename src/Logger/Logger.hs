module Logger.Logger (printLine, printHeader, printStep, printUsage) where
    import Prelude hiding (read,write)
    import System.Environment
    import Machine.Data
    import Engine.Data

    trail :: String -> String -> Int -> String
    trail s c i
        | ((length s) < i)  = trail (s ++ c) c i
        | otherwise         = s

    printStrLine :: String -> Int -> IO ()
    printStrLine s n
        | (n > 0)   = do
            putStr s
            printStrLine s $ n - 1
        | otherwise = putStr "\n"

    printLine :: IO ()
    printLine = printStrLine "*" 80

    getList :: [String] -> String
    getList l = "[ " ++ (getList' l) ++ " ]" where
        getList' :: [String] -> String
        getList' l = case l of
            (s:[])  -> s
            (s:l')  -> s ++ ", " ++ (getList' l')
            []      -> ""

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
            printEmptyLine n
                | (n == 0 || n == 79)   = printString "*" n
                | (n < 80)  = printString " " n
                | otherwise = putStr "\n"
                where
                    printString :: String -> Int -> IO ()
                    printString s n = do
                        putStr s
                        printEmptyLine $ n + 1

            printName :: String -> Int -> IO ()
            printName name n
                | (n == ((80 - length name) `quot` 2)) = do
                    putStr name
                    printName name $ n + 1
                | (n == 0 || n == (80 - length name)) = do
                    putStr "*"
                    printName name $ n + 1
                | (n < 80) = do
                    putStr " "
                    printName name $ n + 1
                | otherwise = putStr "\n"

    printStep :: Engine -> Machine -> ActionTransition -> IO ()
    printStep engine machine t = do
        putStrLn $ (trail (show (step engine)) " " 3) ++ "["++(formatTape (tape engine) (pos engine) 0 30 (blank machine) (initpos engine))++"]" ++ " (\'" ++ (read t) ++ "\', "++ (state engine) ++") -> ( \'" ++ (write t) ++ "\', "++ (to_state t) ++ ", "++ (action t) ++")"
        where
            formatTape :: [String] -> Int -> Int -> Int -> String -> Int -> String
            formatTape tape pos i n blank initpos = case tape of
                (s:ls)  -> (if i == pos then "\x1b[1;32m\x1b[41m" ++ s ++ "\x1b[0m" else if ((i == initpos) && (not (i == 0))) then "\x1b[44m" ++ s ++ "\x1b[0m" else s) ++ (formatTape ls pos (i+1) n blank initpos)
                []      -> if i < n then blank ++ (formatTape tape pos (i + 1) n blank initpos) else ""

    printUsage :: IO ()
    printUsage = do
        name <- getProgName
        putStrLn $ "usage: " ++ (show name) ++ " [-h] jsonfile input\
        \ \n \
        \ positional arguments:\n \
        \ jsonfile              json description of the machine\n \
        \ \n \
        \ input                 input of the machine\n \
        \ \n \
        \ optional arguments: \n \
        \ -h, --help            show this help message and exit"
