import System.Environment
import qualified Data.Map as Map

genMap' arr m = case arr of
  [] -> m
  (c1:c2:a) -> genMap' a (Map.insert c1 c2 m)
genMap arr = genMap' arr (Map.empty)

main = do
  args <- getArgs
  if not ((length args) `mod` 2 == 0) then do
                              print "usage: ./bin k1 d1 k2 d2 ..."
                              return ()
  else do
  let m = (genMap args)
  print m
  print $ "Map.null: " ++ (show (Map.null m))
  print $ "Map.size: " ++ (show (Map.size m))
  print $ "Map.lookup \"abc\": " ++ (show (Map.lookup "abc" m))
  print $ "Map.member \"abc\": " ++ (show (Map.member "abc" m))
  print (Map.delete "abc" m)
  print $ "called Map.delete"
