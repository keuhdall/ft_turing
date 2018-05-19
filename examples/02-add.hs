import System.Environment


main = do
  args <- getArgs
  let a = (read (args !! 0) :: Int)
  let b = (read (args !! 1) :: Int)
  print (a + b)
