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
--             , testCase "lower" test_lower
             , testCase "left" test_left
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
    sizeX = (deltaU `div` 10) - 1
    x = makeTree sizeX 10
    sizeY = ((deltaU - 5) `div` 10) - 1
    y = makeTree sizeY 101
    t = nd 2 (sg 1) (nd 100 (nd 3 Tip x) y)

----------------------------------------------------------------

test_right :: Assertion
test_right = if deltaU < 45
             then True @?= True
             else do
                 unless (valid t) (error "test_right")
                 valid (deleteMin t) @?= True
  where
    sizeX = (deltaU `div` 5) - 5
    x = makeTree sizeX 10
    t = nd 2 (sg 1) (nd 1000 (nd 100 x (sg 101)) (sg 1001))

----------------------------------------------------------------

test_lower :: Assertion
test_lower = do
    putStrLn $ "(" ++ show deltaU ++ "," ++ show ratioU ++ ")"
    print (se,sz,sw,sy)
    putStrLn $ showTree t
    unless (valid t) (error "test_lower")
    let t' = deleteMin t
    putStrLn $ showTree t'
    valid t' @?= True
  where
    (se,sz,sw,sy) = findLow
    e = makeTree se 0
    z = makeTree sz 2000
    w = makeTree sw 4000
    y = makeTree sy 6000
    t = nd 1000 e (nd 5000 (nd 3000 z w) y)

findLow :: (Int,Int,Int,Int)
findLow = fromJust . head . P.filter isJust . P.map findLow' $ [1..]

findLow' :: Int -> Maybe (Int,Int,Int,Int)
findLow' d = if isBal x y && isBal y x && isBal z w && isBal w z
             then Just (e,z,w,y)
             else Nothing
  where
    x = z + w + 1
    y = d + 1
    z = d
    w = ceiling $ (fromIntegral (d + 2)) * r - fromIntegral d
    r :: Float
    r = fromIntegral ratioU / fromIntegral ratioD
    invd :: Float
    invd = fromIntegral deltaD / fromIntegral deltaU
    e = (ceiling $ fromIntegral (x + y + 2) * invd) - 1

----------------------------------------------------------------

test_left :: Assertion
test_left = if deltaU >= 33
             then True @?= True
             else do
                 unless (valid t) (error "test_left")
                 valid (deleteMin t) @?= True
  where
    (sd,sx,sy) = findHigh
    d = makeTree sd 0
    x = makeTree sx 2000
    y = makeTree sy 4000
    t = (nd 1000 d (nd 3000 x y))

findHigh :: (Int,Int,Int)
findHigh = fromJust . head . P.filter isJust . P.map findHigh' $ [1..]

findHigh' :: Int -> Maybe (Int,Int,Int)
findHigh' x = if isBal d e && isBal e d && isBal x y && isBal y x
           then Just (d,x,y)
           else Nothing
  where
    y = floor $ (fromIntegral (x + 1)) * invr
    d = floor $ (fromIntegral (x + y + 2)) * invd
    e = x + y + 1
    invr :: Float
    invr = fromIntegral ratioD / fromIntegral ratioU
    invd :: Float
    invd = fromIntegral deltaD / fromIntegral deltaU

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

