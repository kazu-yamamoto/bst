module Matrix where

import System.Exit
import System.Process

main :: IO ()
main = do
    results <- mapM test [(x,y) | x <- [30], y <- [100,105..215]] -- FIXME
    let res = (concatMap toString . map toRes . filter ok $ results)
           ++ "\n"
           ++ (concatMap toString . map toRes . filter ng $ results)
    writeFile "RES" res
  where
    toString (x,y) = show x ++ " " ++ show y ++ "\n"
    toRes :: (Bool,(Int,Int)) -> (Float,Float)
    toRes (_,(x,y)) = (fromIntegral x / 10, fromIntegral y / 100)
    ok = fst
    ng = not . fst

test :: (Int, Int) -> IO (Bool,(Int, Int))
test (n,m) = do
  system $ delta n ++ " " ++ from ++ " | " ++ ratio m ++ " > " ++ to
  status <- system "runghc -i.. -DMETHOD=3 -DTEST Test.hs --maximum-generated-tests=1000" -- FIXME
  case status of
      ExitSuccess -> return (True,(n,m))
      _           -> return (False,(n,m))
    
delta :: Int -> String
delta n = "sed -e 's/^deltaU = [0-9][0-9]/deltaU = " ++ show n ++  "/'"

ratio :: Int -> String
ratio n = "sed -e 's/^ratioU = [0-9][0-9][0-9]/ratioU = " ++ show n ++  "/'"

from :: String
from = "../Data/SMap/Balance-test.hs"

to :: String
to = "../Data/SMap/Balance.hs"

