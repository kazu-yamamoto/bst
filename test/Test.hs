{-# LANGUAGE CPP #-}

module Test where

import Prelude hiding (lookup,map,filter,null)
import Data.BST.Internal
import Test.Framework (defaultMain, testGroup, Test)
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
--import Test.HUnit hiding (Test, Testable)
import Test.QuickCheck

tests :: [Test]
tests = [ {- testGroup "Test Case" [
               testCase "ticket4242" test_ticket4242
             ]
        , -} testGroup "Property Test" [
               testProperty "validity"     prop_Valid
             , testProperty "single"       prop_Single
             , testProperty "insertValid"  prop_InsertValid
             , testProperty "insertDelete" prop_InsertDelete
             , testProperty "deleteValid"  prop_DeleteValid
             ]
        ]

main :: IO ()
main = defaultMain tests

{-
test_ticket4242 :: Assertion
test_ticket4242 = (valid $ deleteMin $ deleteMin $ fromList [ (i, ()) | i <- [0,2,5,1,6,4,8,9,7,11,10,3] ]) @?= True
-}

{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance (Enum k,Arbitrary a) => Arbitrary (Map k a) where
  arbitrary = sized (arbtree 0 maxkey)
            where maxkey  = 10000

arbtree :: (Enum k,Arbitrary a) => Int -> Int -> Int -> Gen (Map k a)
arbtree lo hi n
  | n <= 0        = return Tip
  | lo >= hi      = return Tip
  | otherwise     = do{ x  <- arbitrary
                      ; i  <- choose (lo,hi)
                      ; m  <- choose (1,30)
                      ; let (ml,mr)  | m==(1::Int)= (1,2)
                                     | m==2       = (2,1)
                                     | m==3       = (1,1)
                                     | otherwise  = (2,2)
                      ; l  <- arbtree lo (i-1) (n `div` ml)
                      ; r  <- arbtree (i+1) hi (n `div` mr)
                      ; return (bin (toEnum i) x l r)
                      }


{--------------------------------------------------------------------
  Valid tree's
--------------------------------------------------------------------}
forValid :: (Show k,Enum k,Show a,Arbitrary a,Testable b) => (Map k a -> b) -> Property
forValid f
  = forAll arbitrary $ \t ->
--    classify (balanced t) "balanced" $
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $
    balanced t ==> f t

forValidIntTree :: Testable a => (Map Int Int -> a) -> Property
forValidIntTree f = forValid f

forValidUnitTree :: Testable a => (Map Int () -> a) -> Property
forValidUnitTree f = forValid f


prop_Valid :: Property
prop_Valid = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Int -> Bool
prop_Single k x = (insert k x empty == singleton k x)

prop_InsertValid :: Int -> Property
prop_InsertValid k = forValidUnitTree $ \t -> valid (insert k () t)

prop_InsertDelete :: Int -> Map Int () -> Property
prop_InsertDelete k t
  = (lookup k t == Nothing) ==> delete k (insert k () t) == t

prop_DeleteValid :: Int -> Property
prop_DeleteValid k
  = forValidUnitTree $ \t -> valid (delete k (insert k () t))
