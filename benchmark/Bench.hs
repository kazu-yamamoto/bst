module Main where

import Random
import Data.SMap hiding (map)
import Progression.Main
import Criterion.Main (bench, bgroup, nf)
import Control.DeepSeq
import Prelude hiding (lookup)

type Key = Int
type Val = ()

seed :: Key
seed = 12345

main :: IO Val
main = do
    let !l1 = [1..1000]   :: [Key]
        !l2 = [1..10000]  :: [Key]
        !l3 = [1..100000] :: [Key]
        !x1 = zip l1 (repeat ())
        !x2 = zip l2 (repeat ())
        !x3 = zip l3 (repeat ())
        !t1 = fromList x1
        !t2 = fromList x2
        !t3 = fromList x3
        !r1 = (take   1000 . randoms . mkStdGen $ seed) :: [Key]
        !r2 = (take  10000 . randoms . mkStdGen $ seed) :: [Key]
        !r3 = (take 100000 . randoms . mkStdGen $ seed) :: [Key]
        !y1 = zip r1 (repeat ())
        !y2 = zip r2 (repeat ())
        !y3 = zip r3 (repeat ())
        !s1 = fromList y1
        !s2 = fromList y2
        !s3 = fromList y3
    defaultMain $
        bgroup "" [
             bench "ins i1000"   $ nf fromList x1
           , bench "ins i10000"  $ nf fromList x2
           , bench "ins i100000" $ nf fromList x3
           , bench "ins r1000"   $ nf fromList y1
           , bench "ins r10000"  $ nf fromList y2
           , bench "ins r100000" $ nf fromList y3
           , bench "del i1000"   $ nf (map (del t1)) l1
           , bench "del i10000"  $ nf (map (del t2)) (skip l2 400)
           , bench "del i100000" $ nf (map (del t3)) (skip l3 8000)
           , bench "del r1000"   $ nf (map (del s1)) r1
           , bench "del r10000"  $ nf (map (del s2)) (skip r2 400)
           , bench "del r100000" $ nf (map (del s3)) (skip r3 8000)
           , bench "lok i1000"   $ nf (map (look t1)) l1
           , bench "lok i10000"  $ nf (map (look t2)) l2
           , bench "lok i100000" $ nf (map (look t3)) l3
           , bench "lok r1000"   $ nf (map (look s1)) r1
           , bench "lok r10000"  $ nf (map (look s2)) r2
           , bench "lok r100000" $ nf (map (look s3)) r3
           ]

instance (NFData k, NFData a) => NFData (Data.SMap.Map k a) where
    rnf = rnf . Data.SMap.toList

del :: Map Key Val -> Key -> Map Key Val
del = flip delete

look :: Map Key Val -> Key -> Maybe Val
look = flip lookup

skip :: [a] -> Int -> [a]
skip xs n = skip' xs 0
  where
    skip' [] _ = []
    skip' (y:ys) c
      | c `mod` n == 0 = y : skip' ys (c+1)
      | otherwise      =     skip' ys (c+1)
