import System.Environment

factorial x
  | x <= 1 = 1
  | otherwise = x * factorial (x - 1)

main = do
  args <- getArgs
  let a = (read (args !! 0) :: Int)
  print (factorial a)
