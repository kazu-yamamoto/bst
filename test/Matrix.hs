module Matrix where

import System.Exit
import System.Process

main :: IO ()
main = mapM test [(x,y) | x <- [10,12..50], y <- [10..21]] >>= print
--main = mapM test [(x,y) | x <- [30], y <- [10..21]] >>= print

test :: (Int, Int) -> IO (Int, Int, String)
test (n,m) = do
  system $ delta n ++ " " ++ from ++ " | " ++ ratio m ++ " > " ++ to
--  status <- system "runghc -i.. -DMETHOD=3 Test.hs --maximum-generated-tests=100"
  status <- (system "runghc -i.. -DMETHOD=3 Test.hs --maximum-generated-tests=1000" `catch` \_ -> return (ExitFailure 0))
  case status of
      ExitSuccess -> return (n,m,"OK")
      _           -> return (n,m,"NG")
    
delta :: Int -> String
delta n = "sed -e 's/^deltaU = [0-9][0-9]/deltaU = " ++ show n ++  " /'"

ratio :: Int -> String
ratio n = "sed -e 's/^ratioU = [0-9][0-9]/ratioU = " ++ show n ++  " /'"

from :: String
from = "../Data/SMap/Balance.hs.orig"

to :: String
to = "../Data/SMap/Balance.hs"

