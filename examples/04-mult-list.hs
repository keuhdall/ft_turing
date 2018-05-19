import System.Environment

mlist :: Int -> [Int] -> [Int]
mlist n [] = []
mlist n (c:l) = [c*n] ++ (mlist n l)

main = do
  args <- getArgs
  let n = (read (args !! 0) :: Int)
  let a = (read (args !! 1) :: [Int])
  print (mlist n a)
