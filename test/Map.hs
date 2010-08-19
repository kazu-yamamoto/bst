{-# LANGUAGE CPP #-}

module Map where

import Prelude hiding (lookup,map,filter,null)
import Data.Map.Internal
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck

tests :: [Test]
tests = [ testGroup "Test Case" [
               testCase "ticket4242" test_ticket4242
             ]
        , testGroup "Property Test" [
               testProperty "fromList"                  prop_fromList
             , testProperty "insert to singleton"       prop_single
             , testProperty "insert"                    prop_insert
             , testProperty "insert then lookup"        prop_lookup
             , testProperty "insert then delete"        prop_insertDelete
             , testProperty "insert then deelete"       prop_insertDelete2
             , testProperty "delete non member"         prop_deleteNonMember
             , testProperty "deleteMin"                 prop_deleteMin
             ]
        ]

main :: IO ()
main = defaultMain tests


test_ticket4242 :: Assertion
test_ticket4242 = (valid $ deleteMin $ deleteMin $ fromList [ (i, ()) | i <- [0,2,5,1,6,4,8,9,7,11,10,3] :: [Int] ]) @?= True

{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance (Ord k,Arbitrary k,Arbitrary a) => Arbitrary (Map k a) where
  arbitrary = do
    ks <- arbitrary
    as <- arbitrary
    return $ fromList $ zip ks as

prop_fromList :: Map Int () -> Bool
prop_fromList t = valid t

prop_single :: Int -> Int -> Bool
prop_single k x = insert k x empty == singleton k x

prop_insert :: Int -> Map Int () -> Bool
prop_insert k t = valid $ insert k () t

prop_lookup :: Int -> Map Int () -> Bool
prop_lookup k t = lookup k (insert k () t) /= Nothing

prop_insertDelete :: Int -> Map Int () -> Bool
prop_insertDelete k t = valid $ delete k (insert k () t)

prop_insertDelete2 :: Int -> Map Int () -> Property
prop_insertDelete2 k t = (lookup k t == Nothing) ==> (delete k (insert k () t) == t)

prop_deleteNonMember :: Int -> Map Int () -> Property
prop_deleteNonMember k t = (lookup k t == Nothing) ==> (delete k t == t)

prop_deleteMin :: Map Int () -> Bool
prop_deleteMin t = valid $ deleteMin $ deleteMin t
