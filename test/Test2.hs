 module Main where

import Data.Maybe
import Control.Monad
import Data.SMap.Types
import Data.SMap.Balance
import Data.SMap.Internal
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, Testable)
import Prelude as P

tests :: [Test]
tests = [ testGroup "Test Case" [
               testCase "upper" test_upper
             , testCase "right" test_right
             , testCase "lower" test_lower
             , testCase "left"  test_left
             ]
        ]

main :: IO ()
main = defaultMain tests

----------------------------------------------------------------

test_upper :: Assertion
test_upper = do
    putStrLn $ "(" ++ show deltaU ++ "," ++ show ratioU ++ ")"
    if deltaU < 25
        then True @?= True
        else do
          unless (valid t) (error "test_upper")
          valid (deleteMin t) @?= True
  where
    x = (deltaU `div` 10) - 1
    tx = makeTree x 10
    y = ((deltaU - 5) `div` 10) - 1
    ty = makeTree y 101
    t = nd 2 (sg 1) (nd 100 (nd 3 Tip tx) ty)

----------------------------------------------------------------

test_right :: Assertion
test_right = if deltaU < 45
             then True @?= True
             else do
                 unless (valid t) (error "test_right")
                 valid (deleteMin t) @?= True
  where
    x = (deltaU `div` 5) - 5
    tx = makeTree x 10
    t = nd 2 (sg 1) (nd 1000 (nd 100 tx (sg 101)) (sg 1001))

----------------------------------------------------------------

test_lower :: Assertion
test_lower = if deltaU * ratioU >= deltaU * ratioD + deltaD * ratioD
             then True @?= True
             else do
                 unless (valid t) (error "test_lower")
                 valid (deleteMin t) @?= True
  where
    (x,y,z,w) = findLow
    tx = makeTree x 0
    ty = makeTree y 2000
    tz = makeTree z 4000
    tw = makeTree w 6000
    t = nd 1000 tx (nd 5000 (nd 3000 ty tz) tw)

findLow :: (Int,Int,Int,Int)
findLow = fromJust . head . P.filter isJust . P.map findLow' $ [1..]

findLow' :: Int -> Maybe (Int,Int,Int,Int)
findLow' z = if largeEnough z && isBal rl w && isBal w rl && isBal y z && isBal z y
             then Just (x,y,z,w)
             else Nothing
  where
    w = (z + 1) * deltaU `div` deltaD
    y = w - 1
    r = y + z + w + 2
    x = (if just then q else q + 1) - 1
    q = (r + 1) * deltaD `div` deltaU
    just = (r + 1) * deltaD `mod` deltaU == 0
    rl = y + z + 1    

largeEnough :: Int -> Bool
largeEnough z = (w + z + 1) * ratioD >= (w + 1) * ratioU
  where
    w = (z + 1) * deltaU `div` deltaD

----------------------------------------------------------------

test_left :: Assertion
test_left = if deltaD * ratioU <= deltaU * ratioD - deltaD * ratioD
            then True @?= True
            else do
                unless (valid t) (error "test_left")
                valid (deleteMin t) @?= True
  where
    (x,y,z) = findHigh
    tx = makeTree x 0
    ty = makeTree y 2000
    tz = makeTree z 4000
    t = nd 1000 tx (nd 3000 tz ty)

findHigh :: (Int,Int,Int)
findHigh = fromJust . head . P.filter isJust . P.map findHigh' $ [1..]

findHigh' :: Int -> Maybe (Int,Int,Int)
findHigh' y = if bigEnough y && isBal x r && isBal r x && isBal y z && isBal z y
              then Just (x,y,z)
              else Nothing
  where
    z = (y + 1) * ratioD `div` ratioU
    x = (y + z + 2) * deltaD `div` deltaU
    r = y + z + 1

bigEnough :: Int -> Bool
bigEnough y = (((y + z + 2) * deltaD `div` deltaU + y + 1) - (z + 1) * deltaU `div` deltaD) > 0
  where
    z = (y + 1) * ratioD `div` ratioU

----------------------------------------------------------------

makeTree :: Size -> Int -> Map Int ()
makeTree siz low = fromList . take siz . zip [low..] $ (repeat ())

nd :: Int -> Map Int () -> Map Int () -> Map Int ()
nd k l r = Bin sz k () l r
  where
    sz = size l + size r + 1

sg :: Int -> Map Int ()
sg k = singleton k ()

isBal :: Int -> Int -> Bool
isBal a b = deltaU * x >= deltaD * y
  where
    x = a + 1
    y = b + 1

----------------------------------------------------------------
