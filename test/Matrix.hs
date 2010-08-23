module Matrix where

import System.Exit
import System.Process

main :: IO ()
main = mapM test [(x,y) | x <- [1..10], y <- [1..10]] >>= print

test :: (Int, Int) -> IO (Int, Int, String)
test (n,m) = do
  system $ delta n ++ " " ++ from ++ " | " ++ ratio m ++ " > " ++ to
--  status <- system "runghc -i.. TestMap.hs --maximum-generated-tests=1000"
  status <- system "runghc -i.. TestMap.hs --maximum-generated-tests=100"  
  case status of
      ExitSuccess -> return (n,m,"OK")
      _           -> return (n,m,"NG")
    
delta :: Int -> String
delta n = "sed -e 's/^delta = [0-9]/delta = " ++ show n ++  " /'"

ratio :: Int -> String
ratio n = "sed -e 's/^ratio = [0-9]/ratio = " ++ show n ++  " /'"

from :: String
from = "../Data/Map/Internal.hs.orig"

to :: String
to = "../Data/Map/Internal.hs"

