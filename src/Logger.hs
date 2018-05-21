module Logger (printLine, printHeader) where
    printLine :: IO ()
    printLine = printLine' 0 where
        printLine' :: Int -> IO ()
        printLine' n =
            if (n < 80) then do
                putStr "*"
                printLine' $ n + 1
            else
                putStr "\n"


    printHeader :: String -> IO ()
    printHeader name = do
        printLine
        printEmptyLine 0
        printName name 0
        printEmptyLine 0
        printLine where
        
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