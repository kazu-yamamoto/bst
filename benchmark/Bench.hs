module Main where

import Data.SMap.Types
import Data.SMap.Balance
import Data.SMap.Internal
import Progression.Main
import Criterion.Main (bench, bgroup, whnf)

main :: IO ()
main = do
    let !t1 = fromList $ zip [1..1000] (repeat ())
        !t2 = fromList $ zip [1..10000] (repeat ())
        !t3 = fromList $ zip [1..100000] (repeat ())
    defaultMain $
        bgroup "insert" [ 
             bench "1000" $ whnf (insert 1001 ()) t1
           , bench "10000" $ whnf (insert 10001 ()) t2
           , bench "100000" $ whnf (insert 100001 ()) t3
           ]

