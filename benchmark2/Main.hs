module Main where

import Data.RMap
--import Data.SMap
import Random

type Key = Int
type Val = ()

seed :: Key
seed = 12345

main :: IO ()
main = do
    let -- !r3 = (take 100000 . randoms . mkStdGen $ seed) :: [Key]
        !l3 = [1..100000] :: [Key]
        !x3 = zip l3 (repeat ())
        !x = toList $ fromList x3
    x `seq` print "Hello"
    