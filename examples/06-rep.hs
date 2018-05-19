import System.Environment

apply n f a
  | n <= 0 = a
  | otherwise = (apply (n - 1) f (f a))

pow2 a = a * 2

main = do
  args <- getArgs
  let n = (read (args !! 0) :: Int)
  print (apply 2 pow2 n)
