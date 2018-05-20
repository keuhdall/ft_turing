import System.Environment

data Fiche = Fiche {name :: String, age :: Int} deriving (Show)

main = do
  args <- getArgs
  if not ((length args) == 1) then do
                              print "usage: ./bin name"
                              return ()
  else do
  let x = (args !! 0)
  let fiche = Fiche {name=x, age=20}
  print (fiche)
