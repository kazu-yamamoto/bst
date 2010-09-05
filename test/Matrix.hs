module Matrix where

import System.Exit
import System.Process

main :: IO ()
main = do
--    results <- mapM test [(x,y) | x <- [20..50], y <- [100,105..215]] -- FIXME
    results <- mapM test pass
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
  status <- system "runghc -i.. -DMETHOD=3 -DTEST Test.hs --maximum-generated-tests=10000" -- FIXME
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

pass :: [(Int,Int)]
pass = [(24,140)
       ,(25,120)
       ,(25,130)
       ,(25,140)
       ,(25,150)
       ,(26,120)
       ,(26,130)
       ,(26,140)
       ,(26,150)
       ,(27,120)
       ,(27,130)
       ,(27,140)
       ,(27,150)
       ,(28,110)
       ,(28,120)
       ,(28,130)
       ,(28,140)
       ,(28,150)
       ,(29,120)
       ,(29,130)
       ,(29,140)
       ,(29,150)
       ,(30,120)
       ,(30,130)
       ,(30,140)
       ,(30,150)
       ,(30,160)
       ,(30,170)
       ,(30,180)
       ,(30,190)
       ,(30,200)
       ,(31,120)
       ,(31,130)
       ,(31,140)
       ,(31,150)
       ,(31,160)
       ,(31,170)
       ,(31,180)
       ,(31,190)
       ,(31,200)
       ,(32,110)
       ,(32,120)
       ,(32,130)
       ,(32,140)
       ,(32,150)
       ,(32,160)
       ,(32,170)
       ,(32,180)
       ,(32,190)
       ,(32,200)
       ,(33,120)
       ,(33,130)
       ,(33,140)
       ,(33,150)
       ,(33,160)
       ,(33,170)
       ,(33,180)
       ,(33,190)
       ,(33,200)
       ,(34,110)
       ,(34,120)
       ,(34,130)
       ,(34,140)
       ,(34,150)
       ,(34,160)
       ,(34,170)
       ,(34,180)
       ,(34,190)
       ,(34,200)
       ,(35,120)
       ,(35,130)
       ,(36,110)
       ,(36,120)
       ,(36,130)
       ,(37,110)
       ,(37,120)
       ,(37,130)
       ,(38,110)
       ,(38,120)
       ,(38,130)
       ,(39,120)
       ,(39,130)
       ,(40,110)
       ,(40,120)
       ,(40,130)
       ,(40,140)
       ,(40,150)
       ,(40,160)
       ,(41,110)
       ,(41,120)
       ,(41,130)
       ,(41,140)
       ,(41,150)
       ,(41,160)
       ,(42,110)
       ,(42,120)
       ,(42,130)
       ,(42,140)
       ,(42,150)
       ,(42,160)
       ,(43,110)
       ,(43,120)
       ,(43,130)
       ,(43,140)
       ,(43,150)
       ,(43,160)
       ,(44,110)
       ,(44,120)
       ,(44,130)
       ,(44,140)
       ,(44,150)
       ,(44,160)
       ,(45,110)
       ,(45,120)]
