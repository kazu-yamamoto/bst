module Main where

import Random
import Data.SMap.Types
import Data.SMap.Internal
import Progression.Main
import Criterion.Main (bench, bgroup, whnf)

main :: IO ()
main = do
    let !l1 = zip [1..1000]   (repeat ()) :: [(Int,())]
        !l2 = zip [1..10000]  (repeat ()) :: [(Int,())]
        !l3 = zip [1..100000] (repeat ()) :: [(Int,())]
        !r1 = zip (take   1000 . randoms . mkStdGen $ 12345) (repeat ()) :: [(Int,())]
        !r2 = zip (take  10000 . randoms . mkStdGen $ 12345) (repeat ()) :: [(Int,())]
        !r3 = zip (take 100000 . randoms . mkStdGen $ 12345) (repeat ()) :: [(Int,())]
        !t1 = fromList l1
        !t2 = fromList l2
        !t3 = fromList l3
    defaultMain $
        bgroup "" [
             bench "ins i1000"   $ whnf fromList l1
           , bench "ins i10000"  $ whnf fromList l2
           , bench "ins i100000" $ whnf fromList l3
           , bench "ins r1000"   $ whnf fromList r1
           , bench "ins r10000"  $ whnf fromList r2
           , bench "ins r100000" $ whnf fromList r3
{-
           , bench "del 1000"   $ whnf (delete 502) t1
           , bench "del 10000"  $ whnf (delete 5002) t2
           , bench "del 100000" $ whnf (delete 50002) t3
-}
           ]
