import System.Environment

loop i n
  | i >= n = print ("DONE")
  | otherwise = do
      print "OK"
      loop (i + 1) n

main = do
  args <- getArgs
  let s = (read (args !! 0) :: Int)
  (loop 0 s)
