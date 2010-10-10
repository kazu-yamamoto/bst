{-# LANGUAGE CPP #-}

module Main where

import Random
#if SCHEME == 1
import Data.SMap hiding (map)
#else
import Data.RMap
#endif
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
        !r1 = (take   1000 . randoms . mkStdGen $ seed) :: [Key]
        !r2 = (take  10000 . randoms . mkStdGen $ seed) :: [Key]
        !r3 = (take 100000 . randoms . mkStdGen $ seed) :: [Key]
        !y1 = zip r1 (repeat ())
        !y2 = zip r2 (repeat ())
        !y3 = zip r3 (repeat ())
    defaultMain $
        bgroup "" [
             bench "ins i10^3" $ nf fromList x1
           , bench "ins i10^4" $ nf fromList x2
           , bench "ins i10^5" $ nf fromList x3
           , bench "ins r10^3" $ nf fromList y1
           , bench "ins r10^4" $ nf fromList y2
           , bench "ins r10^5" $ nf fromList y3
           ]

#if SCHEME == 1
instance (NFData k, NFData v) => NFData (Data.SMap.Map k v)
#else
instance (NFData k, NFData v) => NFData (Data.RMap.RMap k v)
#endif

