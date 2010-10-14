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
               testCase "right" test_right
             , testCase "left"  test_left
             , testCase "lower" test_lower
             , testCase "upper" test_upper
             ]
        ]

main :: IO ()
main = defaultMain tests

----------------------------------------------------------------

test_right :: Assertion
test_right = do 
    putStrLn $ "(" ++ show deltaU ++ "," ++ show ratioU ++ ")"
    unless (deltaU < 45) $ do
        unless (valid t) (error "test_right")
        valid (deleteMin t) @?= True
  where
    x = flor 1 deltaU deltaD
    tx = makeTree x 10
    y = flor 2 deltaU deltaD - flor 1 deltaU deltaD - 4
    ty = makeTree y 3000
    t = nd 2 (sg 1) (nd 2000 (nd 1000 tx (sg 1001)) ty)

----------------------------------------------------------------

test_left :: Assertion
test_left = unless (deltaD * ratioU <= deltaU * ratioD - deltaD * ratioD) $ do
    unless (valid t) (error "test_left")
    valid (deleteMin t) @?= True
  where
    (x,y,z) = findLeft
    tx = makeTree x 0
    ty = makeTree y 2000
    tz = makeTree z 4000
    t = nd 1000 tx (nd 3000 tz ty)

findLeft :: (Int,Int,Int)
findLeft = fromJust . head . P.filter isJust . P.map findLeft' $ [1..]

findLeft' :: Int -> Maybe (Int,Int,Int)
findLeft' y = if largeEnough
              then Just (x,y,z)
              else Nothing
  where
    z = flor (y + 1) ratioD ratioU
    x = ceil (y + z + 2) deltaD deltaU - 1
    largeEnough = deltaU * (z+1) - deltaD * (x + y + 1) < 0

----------------------------------------------------------------

test_lower :: Assertion
test_lower = unless (deltaU * ratioU >= deltaU * ratioD + deltaD * ratioD) $ do
    unless (valid t) (error "test_lower")
    valid (deleteMin t) @?= True
  where
    (x,y,z,w) = findLower
    tx = makeTree x 0
    ty = makeTree y 2000
    tz = makeTree z 4000
    tw = makeTree w 6000
    t = nd 1000 tx (nd 5000 (nd 3000 ty tz) tw)

findLower :: (Int,Int,Int,Int)
findLower = fromJust . head . P.filter isJust . P.map findLower' $ [1..]

findLower' :: Int -> Maybe (Int,Int,Int,Int)
findLower' z = if largeEnough
               then Just (x,y,z,w)
               else Nothing
  where
    w = flor (z + 1) deltaU deltaD
    y = w - 1
    r = y + z + w + 2
    x = ceil (r + 1) deltaD deltaU - 1
    largeEnough = ratioU * (w + 1)  - ratioD * (y + z + 2) < 0 -- xxx equal

----------------------------------------------------------------

test_upper :: Assertion
test_upper = unless (deltaU < 25) $ do
    unless (valid t) (error "test_upper")
    valid (deleteMin t) @?= True
  where
    x = flor 1 deltaU deltaD - 1
    tx = makeTree x 10
    y = flor 1 (deltaU - 5) deltaD - 1 -- 5 is hard coding
    ty = makeTree y 101
    t = nd 2 (sg 1) (nd 100 (nd 3 Tip tx) ty)

----------------------------------------------------------------

makeTree :: Size -> Int -> Map Int ()
makeTree siz low = fromList . take siz . zip [low..] $ (repeat ())

nd :: Int -> Map Int () -> Map Int () -> Map Int ()
nd k l r = Bin sz k () l r
  where
    sz = size l + size r + 1

sg :: Int -> Map Int ()
sg k = singleton k ()

flor :: Int -> Int -> Int -> Int
flor c u d = c * u `div` d

ceil :: Int -> Int -> Int -> Int
ceil c u d = if r == 0 then q else q + 1
  where
    q = c * u `div` d
    r = c * u `mod` d

----------------------------------------------------------------
