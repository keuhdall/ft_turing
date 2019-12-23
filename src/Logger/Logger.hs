{-# LANGUAGE NamedFieldPuns #-}

module Logger.Logger (printHeader, printStep, printUsage) where
  import Prelude hiding (read,write)
  import Data.List (intercalate)
  import System.Environment
  import Machine.Data
  import Engine.Data

  printHeader :: Machine -> IO ()
  printHeader Machine{name,alphabet,states,initial,finals} = do
    putStrLn $ "********************************************************************************\n\
    \*                                                                              *"
    mapM_ printName [0..80]
    putStrLn $ "*                                                                              *\n\
    \********************************************************************************\n\
    \Alphabet: " ++ "[" ++ (intercalate ", " alphabet) ++ "]\n\
    \States: " ++ "[" ++ (intercalate ", " states) ++ "]\n\
    \Initial: " ++ initial ++ "\n\
    \Finals: " ++ "[" ++ (intercalate ", " finals) ++ "]\n\
    \********************************************************************************" where
      printName n
        | (n == ((80 - length name) `quot` 2))  = putStr name
        | (n == 0 || n == (80 - length name))   = putStr "*"
        | n == 80 = putStr "\n"
        | otherwise = putStr " "

  trail :: Int -> String -> String
  trail n s = if n <= 0 then s else s ++ ([0..n-1] >> " ")

  printStep :: Engine -> Machine -> ActionTransition -> IO ()
  printStep Engine{step,pos,initpos,state,tape} Machine{blank} ActionTransition{read,write,to_state,action} = do
    putStrLn $ (trail 2 $ show step) ++ "["++(formatTape tape pos 0 30 blank) initpos++"]" ++ " (\'" ++ read ++ "\', "++ state ++") -> ( \'" ++ write ++ "\', "++ to_state ++ ", "++ action ++")"
    where
        formatTape :: [String] -> Int -> Int -> Int -> String -> Int -> String
        formatTape tape pos i n blank initpos = case tape of
            (s:ls)  -> (if i == pos then "\x1b[1;32m\x1b[41m" ++ s ++ "\x1b[0m" else if ((i == initpos) && (not (i == 0))) then "\x1b[44m" ++ s ++ "\x1b[0m" else s) ++ (formatTape ls pos (i+1) n blank initpos)
            []      -> if i < n then blank ++ (formatTape tape pos (i + 1) n blank initpos) else ""

  printUsage :: IO ()
  printUsage = do
    name <- getProgName
    putStrLn $ "usage: " ++ name ++ " [-h] jsonfile input\
    \ \n \
    \ positional arguments:\n \
    \ jsonfile              json description of the machine\n \
    \ \n \
    \ input                 input of the machine\n \
    \ \n \
    \ optional arguments: \n \
    \ -h, --help            show this help message and exit"
