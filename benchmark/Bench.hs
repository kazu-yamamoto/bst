module Main where

import Random
import Data.SMap hiding (map)
import Progression.Main
import Criterion.Main (bench, bgroup, nf)
import Control.DeepSeq

seed :: Int
seed = 12345

main :: IO ()
main = do
    let !l1 = [1..1000]   :: [Int]
        !l2 = [1..10000]  :: [Int]
        !l3 = [1..100000] :: [Int]
        !x1 = zip l1 (repeat ())
        !x2 = zip l2 (repeat ())
        !x3 = zip l3 (repeat ())
        !r1 = zip (take   1000 . randoms . mkStdGen $ seed) (repeat ()) :: [(Int,())]
        !r2 = zip (take  10000 . randoms . mkStdGen $ seed) (repeat ()) :: [(Int,())]
        !r3 = zip (take 100000 . randoms . mkStdGen $ seed) (repeat ()) :: [(Int,())]
        !t1 = fromList x1
        !t2 = fromList x2
        !t3 = fromList x3
    defaultMain $
        bgroup "" [
             bench "ins i1000"   $ nf fromList x1
           , bench "ins i10000"  $ nf fromList x2
           , bench "ins i100000" $ nf fromList x3
           , bench "ins r1000"   $ nf fromList r1
           , bench "ins r10000"  $ nf fromList r2
           , bench "ins r100000" $ nf fromList r3
           , bench "del i1000"   $ nf (map (del t1)) l1
           , bench "del i10000"  $ nf (map (del t2)) l2
           , bench "del i100000" $ nf (map (del t3)) l3

           ]

instance (NFData k, NFData a) => NFData (Data.SMap.Map k a) where
    rnf = rnf . Data.SMap.toList

del :: Map Int () -> Int -> Map Int ()
del = flip delete
