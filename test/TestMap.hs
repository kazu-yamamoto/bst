{-# LANGUAGE CPP #-}

module TestMap where

import Control.Applicative ((<$>),(<*>))
import Data.List (nub,sort)
import qualified Data.List as L ((\\),intersect)
import Data.Map.Internal
import Prelude hiding (lookup,map,filter,null)
import qualified Prelude as P (map)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck

----------------------------------------------------------------

tests :: [Test]
tests = [ testGroup "Test Case" [
               testCase "ticket4242" test_ticket4242
             ]
        , testGroup "Property Test" [
               testProperty "fromList"             prop_fromList
             , testProperty "insert to singleton"  prop_singleton
             , testProperty "insert"               prop_insert
             , testProperty "insert then lookup"   prop_lookup
             , testProperty "insert then delete"   prop_insertDelete
             , testProperty "insert then delete2"  prop_insertDelete2
             , testProperty "delete non member"    prop_deleteNonMember
             , testProperty "deleteMin"            prop_deleteMin
             , testProperty "deleteMax"            prop_deleteMax
             , testProperty "split then join"      prop_join
             , testProperty "split then merge"     prop_merge
             , testProperty "union"                prop_union
             , testProperty "union singleton"      prop_unionSingleton
             , testProperty "union associative"    prop_unionAssoc
             , testProperty "fromAscList"          prop_ordered
             , testProperty "fromList then toList" prop_list
             , testProperty "unionWith"            prop_unionWith
             , testProperty "unionWith2"           prop_unionWith2
             , testProperty "union sum"            prop_unionSum
             , testProperty "difference"           prop_difference
             , testProperty "difference model"     prop_differenceModel
             , testProperty "intersection"         prop_intersection
             , testProperty "intersection model"   prop_intersectionModel
             ]
        ]

main :: IO ()
main = defaultMain tests

----------------------------------------------------------------
-- Unit tests

test_ticket4242 :: Assertion
test_ticket4242 = (valid $ deleteMin $ deleteMin $ fromList [ (i, ()) | i <- [0,2,5,1,6,4,8,9,7,11,10,3] :: [Int] ]) @?= True

----------------------------------------------------------------
-- QuickCheck

instance (Ord k,Arbitrary k,Arbitrary a) => Arbitrary (Map k a) where
  arbitrary = fromList <$> (zip <$> arbitrary <*> arbitrary)

type UMap = Map Int ()
type IMap = Map Int Int

----------------------------------------------------------------

prop_fromList :: UMap -> Bool
prop_fromList t = valid t

prop_singleton :: Int -> Int -> Bool
prop_singleton k x = insert k x empty == singleton k x

prop_insert :: Int -> UMap -> Bool
prop_insert k t = valid $ insert k () t

prop_lookup :: Int -> UMap -> Bool
prop_lookup k t = lookup k (insert k () t) /= Nothing

prop_insertDelete :: Int -> UMap -> Bool
prop_insertDelete k t = valid $ delete k (insert k () t)

prop_insertDelete2 :: Int -> UMap -> Property
prop_insertDelete2 k t = (lookup k t == Nothing) ==> (delete k (insert k () t) == t)

prop_deleteNonMember :: Int -> UMap -> Property
prop_deleteNonMember k t = (lookup k t == Nothing) ==> (delete k t == t)

prop_deleteMin :: UMap -> Bool
prop_deleteMin t = valid $ deleteMin $ deleteMin t

prop_deleteMax :: UMap -> Bool
prop_deleteMax t = valid $ deleteMax $ deleteMax t

----------------------------------------------------------------

prop_join :: Int -> UMap -> Bool
prop_join k t = let (l,r) = split k t
                in valid (join k () l r)

prop_merge :: Int -> UMap -> Bool
prop_merge k t = let (l,r) = split k t
                 in valid (merge l r)

----------------------------------------------------------------

prop_union :: UMap -> UMap -> Bool
prop_union t1 t2 = valid (union t1 t2)

prop_unionSingleton :: IMap -> Int -> Int -> Bool
prop_unionSingleton t k x = union (singleton k x) t == insert k x t

prop_unionAssoc :: IMap -> IMap -> IMap -> Bool
prop_unionAssoc t1 t2 t3 = union t1 (union t2 t3) == union (union t1 t2) t3

prop_unionWith :: IMap -> IMap -> Bool
prop_unionWith t1 t2 = (union t1 t2 == unionWith (\_ y -> y) t2 t1)

prop_unionWith2 :: IMap -> IMap -> Bool
prop_unionWith2 t1 t2 = valid (unionWithKey (\_ x y -> x+y) t1 t2)

prop_unionSum :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_unionSum xs ys
  = sum (elems (unionWith (+) (fromListWith (+) xs) (fromListWith (+) ys)))
    == (sum (P.map snd xs) + sum (P.map snd ys))

prop_difference :: IMap -> IMap -> Bool
prop_difference t1 t2 = valid (difference t1 t2)

prop_differenceModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_differenceModel xs ys
  = sort (keys (difference (fromListWith (+) xs) (fromListWith (+) ys)))
    == sort ((L.\\) (nub (P.map fst xs))  (nub (P.map fst ys)))

prop_intersection :: IMap -> IMap -> Bool
prop_intersection t1 t2 = valid (intersection t1 t2)

prop_intersectionModel :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_intersectionModel xs ys
  = sort (keys (intersection (fromListWith (+) xs) (fromListWith (+) ys)))
    == sort (nub ((L.intersect) (P.map fst xs) (P.map fst ys)))

----------------------------------------------------------------

prop_ordered :: Property
prop_ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [(x,()) | x <- [0..n::Int]]
    in fromAscList xs == fromList xs

prop_list :: [Int] -> Bool
prop_list xs = (sort (nub xs) == [x | (x,()) <- toList (fromList [(x,()) | x <- xs])])
