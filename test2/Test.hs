module Main where

import Data.RMap.Internal
import Control.Applicative ((<$>),(<*>))
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

instance (Ord k,Arbitrary k,Arbitrary a) => Arbitrary (RMap k a) where
  arbitrary = fromList <$> (zip <$> arbitrary <*> arbitrary)

type UMap = RMap Int ()
type IMap = RMap Int Int
type SMap = RMap Int String

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Property Test" [
               testProperty "fromList"             prop_fromList
             , testProperty "split"                prop_split
             ]
        ]

prop_fromList :: [Int] -> Bool
prop_fromList is = valid t
  where
    t = fromList $ zip is (repeat ())

prop_split :: [Int] -> Bool
prop_split is = valid' lt && valid' gt
  where
    t = fromList $ zip is (repeat ())
    x = is !! (length is `div` 2)
    (lt,gt) = split x (bst t)
