module Main where

import Data.SMap.Types
import Data.SMap.Internal
import Progression.Main
import Criterion.Main (bench, bgroup, whnf)

main :: IO ()
main = do
    let !t1 = fromList $ zip [1..1000] (repeat ()) :: Map Int ()
        !t2 = fromList $ zip [1..10000] (repeat ()) :: Map Int ()
        !t3 = fromList $ zip [1..100000] (repeat ()) :: Map Int ()
    defaultMain $
        bgroup "" [
             bench "ins 1000"   $ whnf (insert 1001 ()) t1
           , bench "ins 10000"  $ whnf (insert 10001 ()) t2
           , bench "ins 100000" $ whnf (insert 100001 ()) t3
           , bench "del 1000"   $ whnf (delete 500) t1
           , bench "del 10000"  $ whnf (delete 5000) t2
           , bench "del 100000" $ whnf (delete 50000) t3
           ]
