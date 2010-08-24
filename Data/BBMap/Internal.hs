{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -XNoBangPatterns #-}

module Data.BBMap.Internal where

import Data.Bits ((.&.), shiftR, shiftL)
import Prelude hiding (lookup,map,filter,null)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..), (<$>))
import Data.Traversable (Traversable(traverse))
import Data.Foldable (Foldable(foldMap))
#ifndef __GLASGOW_HASKELL__
import Data.Typeable ( Typeable, typeOf, typeOfDefault
                     , Typeable1, typeOf1, typeOf1Default)
#endif
import Data.Typeable (Typeable2(..), TyCon, mkTyCon, mkTyConApp)

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Data (Data(..), mkNoRepType, gcast2)
#endif

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: Ord k => BBMap k a -> k -> a
m ! k    = find k m

-- | Same as 'difference'.
(\\) :: Ord k => BBMap k a -> BBMap k b -> BBMap k a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Size balanced trees.
--------------------------------------------------------------------}
-- | A BBMap from keys @k@ to values @a@.
data BBMap k a = Tip
              | Bin {-# UNPACK #-} !Size !k a !(BBMap k a) !(BBMap k a)

type Size     = Int

instance (Ord k) => Monoid (BBMap k v) where
    mempty  = empty
    mappend = union
    mconcat = unions

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance (Data k, Data a, Ord k) => Data (BBMap k a) where
  gfoldl f z m   = z fromList `f` toList m
  toConstr _     = error "toConstr"
  gunfold _ _    = error "gunfold"
  dataTypeOf _   = mkNoRepType "Data.BBMap.BBMap"
  dataCast2 f    = gcast2 f

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.BBMap.null (empty)           == True
-- > Data.BBMap.null (singleton 1 'a') == False

null :: BBMap k a -> Bool
null t
  = case t of
      Tip    -> True
      Bin {} -> False

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3

size :: BBMap k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.BBMap
-- >
-- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
-- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
-- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
-- >
-- > employeeCurrency :: String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing

lookup :: Ord k => k -> BBMap k a -> Maybe a
lookup _ Tip              = Nothing
lookup k (Bin _ kx x l r) = case compare k kx of
    LT -> lookup k l
    GT -> lookup k r
    EQ -> Just x

lookupAssoc :: Ord k => k -> BBMap k a -> Maybe (k,a)
lookupAssoc  k t
  = case t of
      Tip -> Nothing
      Bin _ kx x l r
          -> case compare k kx of
               LT -> lookupAssoc k l
               GT -> lookupAssoc k r
               EQ -> Just (kx,x)

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False

member :: Ord k => k -> BBMap k a -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just _  -> True

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Ord k => k -> BBMap k a -> Bool
notMember k m = not $ member k m

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
find :: Ord k => k -> BBMap k a -> a
find k m
  = case lookup k m of
      Nothing -> error "BBMap.find: element not in the map"
      Just x  -> x

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

findWithDefault :: Ord k => a -> k -> BBMap k a -> a
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x  -> x



{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: BBMap k a
empty = Tip

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: k -> a -> BBMap k a
singleton k x = Bin 1 k x Tip Tip

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

insert :: Ord k => k -> a -> BBMap k a -> BBMap k a
insert kx x Tip = singleton kx x
insert kx x (Bin _ ky y l r) = case compare kx ky of
    GT -> balanceL ky y l (insert kx x r)
    LT -> balanceR ky y (insert kx x l) r
    EQ -> bin kx x l r

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"

insertWith :: Ord k => (a -> a -> a) -> k -> a -> BBMap k a -> BBMap k a
insertWith f k x m
  = insertWithKey (\_ x' y' -> f x' y') k x m

-- | Same as 'insertWith', but the combining function is applied strictly.
insertWith' :: Ord k => (a -> a -> a) -> k -> a -> BBMap k a -> BBMap k a
insertWith' f k x m
  = insertWithKey' (\_ x' y' -> f x' y') k x m


-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> BBMap k a -> BBMap k a
insertWithKey f kx x t
  = case t of
      Tip -> singleton kx x
      Bin sy ky y l r
          -> case compare kx ky of
               LT -> balanceR ky y (insertWithKey f kx x l) r
               GT -> balanceL ky y l (insertWithKey f kx x r)
               EQ -> Bin sy kx (f kx x y) l r

-- | Same as 'insertWithKey', but the combining function is applied strictly.
insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> BBMap k a -> BBMap k a
insertWithKey' f kx x t
  = case t of
      Tip -> singleton kx $! x
      Bin sy ky y l r
          -> case compare kx ky of
               LT -> balanceR ky y (insertWithKey' f kx x l) r
               GT -> balanceL ky y l (insertWithKey' f kx x r)
               EQ -> let x' = f kx x y in seq x' (Bin sy kx x' l r)


-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> BBMap k a -> (Maybe a,BBMap k a)
insertLookupWithKey f kx x t
  = case t of
      Tip -> (Nothing, singleton kx x)
      Bin sy ky y l r
          -> case compare kx ky of
               LT -> let (found,l') = insertLookupWithKey f kx x l
                     in (found,balanceR ky y l' r)
               GT -> let (found,r') = insertLookupWithKey f kx x r
                     in (found,balanceL ky y l r')
               EQ -> (Just y, Bin sy kx (f kx x y) l r)

{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}
-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

delete :: Ord k => k -> BBMap k a -> BBMap k a
delete _ Tip                 = Tip
delete k n@(Bin _ kx _ Tip Tip)
  | kx == k    = Tip
  | otherwise  = n
delete k (Bin _ kx x l r)    = case compare k kx of
    GT -> balanceR kx x l (delete k r)
    LT -> balanceL kx x (delete k l) r
    EQ -> glue l r

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: Ord k => (a -> a) -> k -> BBMap k a -> BBMap k a
adjust f k m
  = adjustWithKey (\_ x -> f x) k m

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: Ord k => (k -> a -> a) -> k -> BBMap k a -> BBMap k a
adjustWithKey f k m
  = updateWithKey (\k' x' -> Just (f k' x')) k m

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: Ord k => (a -> Maybe a) -> k -> BBMap k a -> BBMap k a
update f k m
  = updateWithKey (\_ x -> f x) k m

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> BBMap k a -> BBMap k a
updateWithKey f k t
  = case t of
      Tip -> Tip
      Bin sx kx x l r
          -> case compare k kx of
               LT -> balanceR kx x (updateWithKey f k l) r
               GT -> balanceL kx x l (updateWithKey f k r)
               EQ -> case f kx x of
                       Just x' -> Bin sx kx x' l r
                       Nothing -> glue l r

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> BBMap k a -> (Maybe a,BBMap k a)
updateLookupWithKey f k t
  = case t of
      Tip -> (Nothing,Tip)
      Bin sx kx x l r
          -> case compare k kx of
               LT -> let (found,l') = updateLookupWithKey f k l in (found,balanceR kx x l' r)
               GT -> let (found,r') = updateLookupWithKey f k r in (found,balanceL kx x l r')
               EQ -> case f kx x of
                       Just x' -> (Just x',Bin sx kx x' l r)
                       Nothing -> (Just x,glue l r)

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'BBMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]

alter :: Ord k => (Maybe a -> Maybe a) -> k -> BBMap k a -> BBMap k a
alter f k t
  = case t of
      Tip -> case f Nothing of
               Nothing -> Tip
               Just x -> singleton k x
      Bin sx kx x l r
          -> case compare k kx of
               LT -> balance kx x (alter f k l) r
               GT -> balance kx x l (alter f k r)
               EQ -> case f (Just x) of
                       Just x' -> Bin sx kx x' l r
                       Nothing -> glue l r

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map. Calls 'error' when
-- the key is not a 'member' of the map.
--
-- > findIndex 2 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
-- > findIndex 3 (fromList [(5,"a"), (3,"b")]) == 0
-- > findIndex 5 (fromList [(5,"a"), (3,"b")]) == 1
-- > findIndex 6 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map

findIndex :: Ord k => k -> BBMap k a -> Int
findIndex k t
  = case lookupIndex k t of
      Nothing  -> error "BBMap.findIndex: element is not in the map"
      Just idx -> idx

-- | /O(log n)/. Lookup the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map.
--
-- > isJust (lookupIndex 2 (fromList [(5,"a"), (3,"b")]))   == False
-- > fromJust (lookupIndex 3 (fromList [(5,"a"), (3,"b")])) == 0
-- > fromJust (lookupIndex 5 (fromList [(5,"a"), (3,"b")])) == 1
-- > isJust (lookupIndex 6 (fromList [(5,"a"), (3,"b")]))   == False

lookupIndex :: Ord k => k -> BBMap k a -> Maybe Int
lookupIndex k t = f 0 t
  where
    f _   Tip  = Nothing
    f idx (Bin _ kx _ l r)
      = case compare k kx of
          LT -> f idx l
          GT -> f (idx + size l + 1) r
          EQ -> Just (idx + size l)

-- | /O(log n)/. Retrieve an element by /index/. Calls 'error' when an
-- invalid index is used.
--
-- > elemAt 0 (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > elemAt 1 (fromList [(5,"a"), (3,"b")]) == (5, "a")
-- > elemAt 2 (fromList [(5,"a"), (3,"b")])    Error: index out of range

elemAt :: Int -> BBMap k a -> (k,a)
elemAt _ Tip = error "BBMap.elemAt: index out of range"
elemAt i (Bin _ kx x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> (kx,x)
  where
    sizeL = size l

-- | /O(log n)/. Update the element at /index/. Calls 'error' when an
-- invalid index is used.
--
-- > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "x"), (5, "a")]
-- > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "x")]
-- > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range

updateAt :: (k -> a -> Maybe a) -> Int -> BBMap k a -> BBMap k a
updateAt _ _ Tip  = error "BBMap.updateAt: index out of range"
updateAt f i (Bin sx kx x l r)
  = case compare i sizeL of
      LT -> balanceR kx x (updateAt f i l) r
      GT -> balanceL kx x l (updateAt f (i-sizeL-1) r)
      EQ -> case f kx x of
              Just x' -> Bin sx kx x' l r
              Nothing -> glue l r
  where
    sizeL = size l

-- | /O(log n)/. Delete the element at /index/.
-- Defined as (@'deleteAt' i map = 'updateAt' (\k x -> 'Nothing') i map@).
--
-- > deleteAt 0  (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > deleteAt 1  (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > deleteAt 2 (fromList [(5,"a"), (3,"b")])     Error: index out of range
-- > deleteAt (-1) (fromList [(5,"a"), (3,"b")])  Error: index out of range

deleteAt :: Int -> BBMap k a -> BBMap k a
deleteAt i m
  = updateAt (\_ _ -> Nothing) i m


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
-- | /O(log n)/. The minimal key of the map. Calls 'error' is the map is empty.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element

findMin :: BBMap k a -> (k,a)
findMin (Bin _ kx x Tip _)  = (kx,x)
findMin (Bin _ _  _ l _)    = findMin l
findMin Tip                 = error "BBMap.findMin: empty map has no minimal element"

-- | /O(log n)/. The maximal key of the map. Calls 'error' is the map is empty.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element

findMax :: BBMap k a -> (k,a)
findMax (Bin _ kx x _ Tip)  = (kx,x)
findMax (Bin _ _  _ _ r)    = findMax r
findMax Tip                 = error "BBMap.findMax: empty map has no maximal element"

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty

deleteMin :: BBMap k a -> BBMap k a
deleteMin (Bin _ _  _ Tip r)  = r
deleteMin (Bin _ kx x l r)    = balanceL kx x (deleteMin l) r
deleteMin Tip                 = Tip

-- | /O(log n)/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
-- > deleteMax empty == empty

deleteMax :: BBMap k a -> BBMap k a
deleteMax (Bin _ _  _ l Tip)  = l
deleteMax (Bin _ kx x l r)    = balanceR kx x l (deleteMax r)
deleteMax Tip                 = Tip

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> Maybe a) -> BBMap k a -> BBMap k a
updateMin f m
  = updateMinWithKey (\_ x -> f x) m

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> Maybe a) -> BBMap k a -> BBMap k a
updateMax f m
  = updateMaxWithKey (\_ x -> f x) m


-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (k -> a -> Maybe a) -> BBMap k a -> BBMap k a
updateMinWithKey f t
  = case t of
      Bin sx kx x Tip r  -> case f kx x of
                              Nothing -> r
                              Just x' -> Bin sx kx x' Tip r
      Bin _ kx x l r     -> balanceR kx x (updateMinWithKey f l) r
      Tip                -> Tip

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (k -> a -> Maybe a) -> BBMap k a -> BBMap k a
updateMaxWithKey f t
  = case t of
      Bin sx kx x l Tip  -> case f kx x of
                              Nothing -> l
                              Just x' -> Bin sx kx x' l Tip
      Bin _ kx x l r     -> balanceL kx x l (updateMaxWithKey f r)
      Tip                -> Tip

-- | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: BBMap k a -> Maybe ((k,a), BBMap k a)
minViewWithKey Tip = Nothing
minViewWithKey x = Just (deleteFindMin x)

-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: BBMap k a -> Maybe ((k,a), BBMap k a)
maxViewWithKey Tip = Nothing
maxViewWithKey x = Just (deleteFindMax x)

-- | /O(log n)/. Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > minView (fromList [(5,"a"), (3,"b")]) == Just ("b", singleton 5 "a")
-- > minView empty == Nothing

minView :: BBMap k a -> Maybe (a, BBMap k a)
minView Tip = Nothing
minView x = Just (first snd $ deleteFindMin x)

-- | /O(log n)/. Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
--
-- > maxView (fromList [(5,"a"), (3,"b")]) == Just ("a", singleton 3 "b")
-- > maxView empty == Nothing

maxView :: BBMap k a -> Maybe (a, BBMap k a)
maxView Tip = Nothing
maxView x = Just (first snd $ deleteFindMax x)

-- Update the 1st component of a tuple (special case of Control.Arrow.first)
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: Ord k => [BBMap k a] -> BBMap k a
unions ts
  = foldlStrict union empty ts

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: Ord k => (a->a->a) -> [BBMap k a] -> BBMap k a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts

-- | /O(n+m)/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset \``union`\` smallset).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: Ord k => BBMap k a -> BBMap k a -> BBMap k a
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = hedgeUnionL (const LT) (const GT) t1 t2

-- left-biased hedge union
hedgeUnionL :: Ord a
            => (a -> Ordering) -> (a -> Ordering) -> BBMap a b -> BBMap a b
            -> BBMap a b
hedgeUnionL _     _     t1 Tip
  = t1
hedgeUnionL cmplo cmphi Tip (Bin _ kx x l r)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionL cmplo cmphi (Bin _ kx x l r) t2
  = join kx x (hedgeUnionL cmplo cmpkx l (trim cmplo cmpkx t2))
              (hedgeUnionL cmpkx cmphi r (trim cmpkx cmphi t2))
  where
    cmpkx k  = compare kx k

{-
XXX unused code

-- right-biased hedge union
hedgeUnionR :: Ord a
            => (a -> Ordering) -> (a -> Ordering) -> BBMap a b -> BBMap a b
            -> BBMap a b
hedgeUnionR _     _     t1 Tip
  = t1
hedgeUnionR cmplo cmphi Tip (Bin _ kx x l r)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionR cmplo cmphi (Bin _ kx x l r) t2
  = join kx newx (hedgeUnionR cmplo cmpkx l lt)
                 (hedgeUnionR cmpkx cmphi r gt)
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t2
    (found,gt)  = trimLookupLo kx cmphi t2
    newx        = case found of
                    Nothing -> x
                    Just (_,y) -> y
-}

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}
-- | /O(n+m)/. Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

unionWith :: Ord k => (a -> a -> a) -> BBMap k a -> BBMap k a -> BBMap k a
unionWith f m1 m2
  = unionWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/.
-- Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset \``union`\` smallset).
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

unionWithKey :: Ord k => (k -> a -> a -> a) -> BBMap k a -> BBMap k a -> BBMap k a
unionWithKey _ Tip t2  = t2
unionWithKey _ t1 Tip  = t1
unionWithKey f t1 t2 = hedgeUnionWithKey f (const LT) (const GT) t1 t2

hedgeUnionWithKey :: Ord a
                  => (a -> b -> b -> b)
                  -> (a -> Ordering) -> (a -> Ordering)
                  -> BBMap a b -> BBMap a b
                  -> BBMap a b
hedgeUnionWithKey _ _     _     t1 Tip
  = t1
hedgeUnionWithKey _ cmplo cmphi Tip (Bin _ kx x l r)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionWithKey f cmplo cmphi (Bin _ kx x l r) t2
  = join kx newx (hedgeUnionWithKey f cmplo cmpkx l lt)
                 (hedgeUnionWithKey f cmpkx cmphi r gt)
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t2
    (found,gt)  = trimLookupLo kx cmphi t2
    newx        = case found of
                    Nothing -> x
                    Just (_,y) -> f kx x y

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: Ord k => BBMap k a -> BBMap k b -> BBMap k a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff (const LT) (const GT) t1 t2

hedgeDiff :: Ord a
          => (a -> Ordering) -> (a -> Ordering) -> BBMap a b -> BBMap a c
          -> BBMap a b
hedgeDiff _     _     Tip _
  = Tip
hedgeDiff cmplo cmphi (Bin _ kx x l r) Tip
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiff cmplo cmphi t (Bin _ kx _ l r)
  = merge (hedgeDiff cmplo cmpkx (trim cmplo cmpkx t) l)
          (hedgeDiff cmpkx cmphi (trim cmpkx cmphi t) r)
  where
    cmpkx k = compare kx k

-- | /O(n+m)/. Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"

differenceWith :: Ord k => (a -> b -> Maybe a) -> BBMap k a -> BBMap k b -> BBMap k a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> BBMap k a -> BBMap k b -> BBMap k a
differenceWithKey _ Tip _   = Tip
differenceWithKey _ t1 Tip  = t1
differenceWithKey f t1 t2   = hedgeDiffWithKey f (const LT) (const GT) t1 t2

hedgeDiffWithKey :: Ord a
                 => (a -> b -> c -> Maybe b)
                 -> (a -> Ordering) -> (a -> Ordering)
                 -> BBMap a b -> BBMap a c
                 -> BBMap a b
hedgeDiffWithKey _ _     _     Tip _
  = Tip
hedgeDiffWithKey _ cmplo cmphi (Bin _ kx x l r) Tip
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiffWithKey f cmplo cmphi t (Bin _ kx x l r)
  = case found of
      Nothing -> merge tl tr
      Just (ky,y) ->
          case f ky y x of
            Nothing -> merge tl tr
            Just z  -> join ky z tl tr
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t
    (found,gt)  = trimLookupLo kx cmphi t
    tl          = hedgeDiffWithKey f cmplo cmpkx lt l
    tr          = hedgeDiffWithKey f cmpkx cmphi gt r



{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: Ord k => BBMap k a -> BBMap k b -> BBMap k a
intersection m1 m2
  = intersectionWithKey (\_ x _ -> x) m1 m2

-- | /O(n+m)/. Intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: Ord k => (a -> b -> c) -> BBMap k a -> BBMap k b -> BBMap k c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. Intersection with a combining function.
-- Intersection is more efficient on (bigset \``intersection`\` smallset).
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

--intersectionWithKey :: Ord k => (k -> a -> b -> c) -> BBMap k a -> BBMap k b -> BBMap k c
--intersectionWithKey f Tip t = Tip
--intersectionWithKey f t Tip = Tip
--intersectionWithKey f t1 t2 = intersectWithKey f t1 t2
--
--intersectWithKey f Tip t = Tip
--intersectWithKey f t Tip = Tip
--intersectWithKey f t (Bin _ kx x l r)
--  = case found of
--      Nothing -> merge tl tr
--      Just y  -> join kx (f kx y x) tl tr
--  where
--    (lt,found,gt) = splitLookup kx t
--    tl            = intersectWithKey f lt l
--    tr            = intersectWithKey f gt r

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> BBMap k a -> BBMap k b -> BBMap k c
intersectionWithKey _ Tip _ = Tip
intersectionWithKey _ _ Tip = Tip
intersectionWithKey f t1@(Bin s1 k1 x1 l1 r1) t2@(Bin s2 k2 x2 l2 r2) =
   if s1 >= s2 then
      let (lt,found,gt) = splitLookupWithKey k2 t1
          tl            = intersectionWithKey f lt l2
          tr            = intersectionWithKey f gt r2
      in case found of
      Just (k,x) -> join k (f k x x2) tl tr
      Nothing -> merge tl tr
   else let (lt,found,gt) = splitLookup k1 t2
            tl            = intersectionWithKey f l1 lt
            tr            = intersectionWithKey f r1 gt
      in case found of
      Just x -> join k1 (f k1 x1 x) tl tr
      Nothing -> merge tl tr



{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
--
isSubmapOf :: (Ord k,Eq a) => BBMap k a -> BBMap k a -> Bool
isSubmapOf m1 m2
  = isSubmapOfBy (==) m1 m2

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

 > isSubmapOfBy (==) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<=) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)])

 But the following are all 'False':

 > isSubmapOfBy (==) (fromList [('a',2)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<)  (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1)])

-}
isSubmapOfBy :: Ord k => (a->b->Bool) -> BBMap k a -> BBMap k b -> Bool
isSubmapOfBy f t1 t2
  = (size t1 <= size t2) && (submap' f t1 t2)

submap' :: Ord a => (b -> c -> Bool) -> BBMap a b -> BBMap a c -> Bool
submap' _ Tip _ = True
submap' _ _ Tip = False
submap' f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y  -> f x y && submap' f l lt && submap' f r gt
  where
    (lt,found,gt) = splitLookup kx t

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Ord k,Eq a) => BBMap k a -> BBMap k a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])

-}
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> BBMap k a -> BBMap k b -> Bool
isProperSubmapOfBy f t1 t2
  = (size t1 < size t2) && (submap' f t1 t2)

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy the predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: Ord k => (a -> Bool) -> BBMap k a -> BBMap k a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: Ord k => (k -> a -> Bool) -> BBMap k a -> BBMap k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x    = join kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)


-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: Ord k => (a -> Bool) -> BBMap k a -> (BBMap k a,BBMap k a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: Ord k => (k -> a -> Bool) -> BBMap k a -> (BBMap k a,BBMap k a)
partitionWithKey _ Tip = (Tip,Tip)
partitionWithKey p (Bin _ kx x l r)
  | p kx x    = (join kx x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,join kx x l2 r2)
  where
    (l1,l2) = partitionWithKey p l
    (r1,r2) = partitionWithKey p r

-- | /O(n)/. BBMap values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: Ord k => (a -> Maybe b) -> BBMap k a -> BBMap k b
mapMaybe f m
  = mapMaybeWithKey (\_ x -> f x) m

-- | /O(n)/. BBMap keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> BBMap k a -> BBMap k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ kx x l r) = case f kx x of
  Just y  -> join kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
  Nothing -> merge (mapMaybeWithKey f l) (mapMaybeWithKey f r)

-- | /O(n)/. BBMap values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: Ord k => (a -> Either b c) -> BBMap k a -> (BBMap k b, BBMap k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | /O(n)/. BBMap keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: Ord k =>
  (k -> a -> Either b c) -> BBMap k a -> (BBMap k b, BBMap k c)
mapEitherWithKey _ Tip = (Tip, Tip)
mapEitherWithKey f (Bin _ kx x l r) = case f kx x of
  Left y  -> (join kx y l1 r1, merge l2 r2)
  Right z -> (merge l1 r1, join kx z l2 r2)
 where
    (l1,l2) = mapEitherWithKey f l
    (r1,r2) = mapEitherWithKey f r

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. BBMap a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> BBMap k a -> BBMap k b
map f m
  = mapWithKey (\_ x -> f x) m

-- | /O(n)/. BBMap a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (k -> a -> b) -> BBMap k a -> BBMap k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r)
  = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> BBMap k b -> (a,BBMap k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> BBMap k b -> (a,BBMap k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function 'mapAccumL' threads an accumulating
-- argument throught the map in ascending order of keys.
mapAccumL :: (a -> k -> b -> (a,c)) -> a -> BBMap k b -> (a,BBMap k c)
mapAccumL f a t
  = case t of
      Tip -> (a,Tip)
      Bin sx kx x l r
          -> let (a1,l') = mapAccumL f a l
                 (a2,x') = f a1 kx x
                 (a3,r') = mapAccumL f a2 r
             in (a3,Bin sx kx x' l' r')

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> k -> b -> (a,c)) -> a -> BBMap k b -> (a,BBMap k c)
mapAccumRWithKey f a t
  = case t of
      Tip -> (a,Tip)
      Bin sx kx x l r
          -> let (a1,r') = mapAccumRWithKey f a r
                 (a2,x') = f a1 kx x
                 (a3,l') = mapAccumRWithKey f a2 l
             in (a3,Bin sx kx x' l' r')

-- | /O(n*log n)/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the smallest of
-- these keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: Ord k2 => (k1->k2) -> BBMap k1 a -> BBMap k2 a
mapKeys = mapKeysWith (\x _ -> x)

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1->k2) -> BBMap k1 a -> BBMap k2 a
mapKeysWith c f = fromListWith c . List.map fFirst . toList
    where fFirst (x,y) = (f x, y)


-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
-- > valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) == True
-- > valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) == False

mapKeysMonotonic :: (k1->k2) -> BBMap k1 a -> BBMap k2 a
mapKeysMonotonic _ Tip = Tip
mapKeysMonotonic f (Bin sz k x l r) =
    Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Fold the values in the map, such that
-- @'fold' f z == 'Prelude.foldr' f z . 'elems'@.
-- For example,
--
-- > elems map = fold (:) [] map
--
-- > let f a len = len + (length a)
-- > fold f 0 (fromList [(5,"a"), (3,"bbb")]) == 4

fold :: (a -> b -> b) -> b -> BBMap k a -> b
fold f z m
  = foldWithKey (\_ x' z' -> f x' z') z m

-- | /O(n)/. Fold the keys and values in the map, such that
-- @'foldWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
-- For example,
--
-- > keys map = foldWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldWithKey f "BBMap: " (fromList [(5,"a"), (3,"b")]) == "BBMap: (5:a)(3:b)"
--
-- This is identical to 'foldrWithKey', and you should use that one instead of
-- this one.  This name is kept for backward compatibility.

foldWithKey :: (k -> a -> b -> b) -> b -> BBMap k a -> b
foldWithKey f z t
  = foldrWithKey f z t

{-
XXX unused code

-- | /O(n)/. In-order fold.
foldi :: (k -> a -> b -> b -> b) -> b -> BBMap k a -> b
foldi _ z Tip               = z
foldi f z (Bin _ kx x l r)  = f kx x (foldi f z l) (foldi f z r)
-}

-- | /O(n)/. Post-order fold.  The function will be applied from the lowest
-- value to the highest.
foldrWithKey :: (k -> a -> b -> b) -> b -> BBMap k a -> b
foldrWithKey _ z Tip              = z
foldrWithKey f z (Bin _ kx x l r) =
    foldrWithKey f (f kx x (foldrWithKey f z r)) l


-- | /O(n)/. Pre-order fold.  The function will be applied from the highest
-- value to the lowest.
foldlWithKey :: (b -> k -> a -> b) -> b -> BBMap k a -> b
foldlWithKey _ z Tip              = z
foldlWithKey f z (Bin _ kx x l r) =
    foldlWithKey f (f (foldlWithKey f z l) kx x) r

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: BBMap k a -> [a]
elems m
  = [x | (_,x) <- assocs m]

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: BBMap k a -> [k]
keys m
  = [k | (k,_) <- assocs m]

-- | /O(n)/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty

keysSet :: BBMap k a -> Set.Set k
keysSet m = Set.fromDistinctAscList (keys m)

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: BBMap k a -> [(k,a)]
assocs m
  = toList m

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}
-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

fromList :: Ord k => [(k,a)] -> BBMap k a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insert k x t

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty

fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> BBMap k a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
-- > fromListWithKey f [] == empty

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> BBMap k a
fromListWithKey f xs
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | /O(n)/. Convert to a list of key\/value pairs.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: BBMap k a -> [(k,a)]
toList t      = toAscList t

-- | /O(n)/. Convert to an ascending list.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: BBMap k a -> [(k,a)]
toAscList t   = foldrWithKey (\k x xs -> (k,x):xs) [] t

-- | /O(n)/. Convert to a descending list.
toDescList :: BBMap k a -> [(k,a)]
toDescList t  = foldlWithKey (\xs k x -> (k,x):xs) [] t

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscList :: Eq k => [(k,a)] -> BBMap k a
fromAscList xs
  = fromAscListWithKey (\_ x _ -> x) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscListWith :: Eq k => (a -> a -> a) -> [(k,a)] -> BBMap k a
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) == True
-- > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False

fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> BBMap k a
fromAscListWithKey f xs
  = fromDistinctAscList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq _ xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,zz) (x@(kx,xx):xs')
    | kx==kz    = let yy = f kx xx zz in combineEq' (kx,yy) xs'
    | otherwise = z:combineEq' x xs'


-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
-- > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False

fromDistinctAscList :: [(k,a)] -> BBMap k a
fromDistinctAscList xs
  = build const (length xs) xs
  where
    -- 1) use continutations so that we use heap space instead of stack space.
    -- 2) special case for n==5 to build bushier trees.
    build c 0 xs'  = c Tip xs'
    build c 5 xs'  = case xs' of
                       ((k1,x1):(k2,x2):(k3,x3):(k4,x4):(k5,x5):xx)
                            -> c (bin k4 x4 (bin k2 x2 (singleton k1 x1) (singleton k3 x3)) (singleton k5 x5)) xx
                       _ -> error "fromDistinctAscList build"
    build c n xs'  = seq nr $ build (buildR nr c) nl xs'
                   where
                     nl = n `div` 2
                     nr = n - nl - 1

    buildR n c l ((k,x):ys) = build (buildB l k x c) n ys
    buildR _ _ _ []         = error "fromDistinctAscList buildR []"
    buildB l k x c r zs     = c (bin k x l r) zs


{--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a comparison function as argument to
  allow comparisons against infinite values. A function [cmplo k]
  should be read as [compare lo k].

  [trim cmplo cmphi t]  A tree that is either empty or where [cmplo k == LT]
                        and [cmphi k == GT] for the key [k] of the root.
  [filterGt cmp t]      A tree where for all keys [k]. [cmp k == LT]
  [filterLt cmp t]      A tree where for all keys [k]. [cmp k == GT]

  [split k t]           Returns two trees [l] and [r] where all keys
                        in [l] are <[k] and all keys in [r] are >[k].
  [splitLookup k t]     Just like [split] but also returns whether [k]
                        was found in the tree.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  [trim lo hi t] trims away all subtrees that surely contain no
  values between the range [lo] to [hi]. The returned tree is either
  empty or the key of the root is between @lo@ and @hi@.
--------------------------------------------------------------------}
trim :: (k -> Ordering) -> (k -> Ordering) -> BBMap k a -> BBMap k a
trim _     _     Tip = Tip
trim cmplo cmphi t@(Bin _ kx _ l r)
  = case cmplo kx of
      LT -> case cmphi kx of
              GT -> t
              _  -> trim cmplo cmphi l
      _  -> trim cmplo cmphi r

trimLookupLo :: Ord k => k -> (k -> Ordering) -> BBMap k a -> (Maybe (k,a), BBMap k a)
trimLookupLo _  _     Tip = (Nothing,Tip)
trimLookupLo lo cmphi t@(Bin _ kx x l r)
  = case compare lo kx of
      LT -> case cmphi kx of
              GT -> (lookupAssoc lo t, t)
              _  -> trimLookupLo lo cmphi l
      GT -> trimLookupLo lo cmphi r
      EQ -> (Just (kx,x),trim (compare lo) cmphi r)


{--------------------------------------------------------------------
  [filterGt k t] filter all keys >[k] from tree [t]
  [filterLt k t] filter all keys <[k] from tree [t]
--------------------------------------------------------------------}
filterGt :: Ord k => (k -> Ordering) -> BBMap k a -> BBMap k a
filterGt _   Tip = Tip
filterGt cmp (Bin _ kx x l r)
  = case cmp kx of
      LT -> join kx x (filterGt cmp l) r
      GT -> filterGt cmp r
      EQ -> r

filterLt :: Ord k => (k -> Ordering) -> BBMap k a -> BBMap k a
filterLt _   Tip = Tip
filterLt cmp (Bin _ kx x l r)
  = case cmp kx of
      LT -> filterLt cmp l
      GT -> join kx x l (filterLt cmp r)
      EQ -> l

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: Ord k => k -> BBMap k a -> (BBMap k a,BBMap k a)
split _ Tip = (Tip,Tip)
split k (Bin _ kx x l r)
  = case compare k kx of
      LT -> let (lt,gt) = split k l in (lt,join kx x gt r)
      GT -> let (lt,gt) = split k r in (join kx x l lt,gt)
      EQ -> (l,r)

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)

splitLookup :: Ord k => k -> BBMap k a -> (BBMap k a,Maybe a,BBMap k a)
splitLookup _ Tip = (Tip,Nothing,Tip)
splitLookup k (Bin _ kx x l r)
  = case compare k kx of
      LT -> let (lt,z,gt) = splitLookup k l in (lt,z,join kx x gt r)
      GT -> let (lt,z,gt) = splitLookup k r in (join kx x l lt,z,gt)
      EQ -> (l,Just x,r)

-- | /O(log n)/.
splitLookupWithKey :: Ord k => k -> BBMap k a -> (BBMap k a,Maybe (k,a),BBMap k a)
splitLookupWithKey _ Tip = (Tip,Nothing,Tip)
splitLookupWithKey k (Bin _ kx x l r)
  = case compare k kx of
      LT -> let (lt,z,gt) = splitLookupWithKey k l in (lt,z,join kx x gt r)
      GT -> let (lt,z,gt) = splitLookupWithKey k r in (join kx x l lt,z,gt)
      EQ -> (l,Just (kx, x),r)

{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Ord k => k -> a -> BBMap k a -> BBMap k a -> BBMap k a
join kx x Tip r  = insertMin kx x r
join kx x l Tip  = insertMax kx x l
join kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | isBalanced l r && isBalanced r l = bin kx x l r
  | isBalanced l r                   = balanceL ky y ly (join kx x ry r) -- xxx
  | otherwise                        = balanceR kz z (join kx x l lz) rz -- xxx

-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: k -> a -> BBMap k a -> BBMap k a
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceL ky y l (insertMax kx x r)

insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceR ky y (insertMin kx x l) r

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: BBMap k a -> BBMap k a -> BBMap k a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | isBalanced l r && isBalanced r l = glue l r
  | isBalanced l r                   = balanceL kx x lx (merge rx r)
  | otherwise                        = balanceR ky y (merge l ly) ry

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: BBMap k a -> BBMap k a -> BBMap k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let ((km,m),l') = deleteFindMax l
                      in balanceL km m l' r
  | otherwise       = let ((km,m),r') = deleteFindMin r
                      in balanceR km m l r'


-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: BBMap k a -> ((k,a),BBMap k a)
deleteFindMin (Bin _ k x Tip r) = ((k,x),r)
deleteFindMin (Bin _ k x l r)   = let (km,l') = deleteFindMin l
                                  in (km,balanceL k x l' r)
deleteFindMin Tip               = (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: BBMap k a -> ((k,a),BBMap k a)
deleteFindMax (Bin _ k x l Tip) = ((k,x),l)
deleteFindMax (Bin _ k x l r)   = let (km,r') = deleteFindMax r
                                  in (km,balanceR k x l r')
deleteFindMax Tip               = (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)

----------------------------------------------------------------

isBalanced :: BBMap k a -> BBMap k a -> Bool
isBalanced a b = 2 * y * y <= (x + y) * (x + y)
  where
    x = size a + 1
    y = size b + 1

isSingle :: BBMap k a -> BBMap k a -> Bool
isSingle a b = 2 * (x + y) * (x + y) < (x + 2 * y) * (x + 2 * y)
  where
    x = size a + 1
    y = size b + 1

----------------------------------------------------------------

balanceL :: k -> a -> BBMap k a -> BBMap k a -> BBMap k a
balanceL k x l r
  | isBalanced l r = bin k x l r
  | otherwise      = rotateL k x l r

balanceR :: k -> a -> BBMap k a -> BBMap k a -> BBMap k a
balanceR k x l r
  | isBalanced r l = bin k x l r
  | otherwise      = rotateR k x l r

balance :: k -> a -> BBMap k a -> BBMap k a -> BBMap k a
balance k x l r
  | isBalanced l r && isBalanced r l = bin k x l r
  | size l > size r                  = rotateR k x l r
  | otherwise                        = rotateL k x l r

----------------------------------------------------------------

rotateL :: a -> b -> BBMap a b -> BBMap a b -> BBMap a b
rotateL k x l r@(Bin _ _ _ rl rr)
  | isSingle rl rr = singleL k x l r
  | otherwise      = doubleL k x l r
rotateL _ _ _ _         = error "rotateL"

rotateR :: a -> b -> BBMap a b -> BBMap a b -> BBMap a b
rotateR k x l@(Bin _ _ _ ll lr) r
  | isSingle lr ll = singleR k x l r
  | otherwise      = doubleR k x l r
rotateR _ _ _ _         = error "rotateR"

----------------------------------------------------------------
-- basic rotations
singleL :: a -> b -> BBMap a b -> BBMap a b -> BBMap a b
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ _ = error "singleL"

singleR :: a -> b -> BBMap a b -> BBMap a b -> BBMap a b
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ _ _ = error "singleR"

doubleL :: a -> b -> BBMap a b -> BBMap a b -> BBMap a b
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"

doubleR :: a -> b -> BBMap a b -> BBMap a b -> BBMap a b
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: k -> a -> BBMap k a -> BBMap k a -> BBMap k a
bin k x l r = Bin (size l + size r + 1) k x l r

{--------------------------------------------------------------------
  Eq converts the tree to a list. In a lazy setting, this
  actually seems one of the faster methods to compare two trees
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance (Eq k,Eq a) => Eq (BBMap k a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance (Ord k, Ord v) => Ord (BBMap k v) where
    compare m1 m2 = compare (toAscList m1) (toAscList m2)

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}
instance Functor (BBMap k) where
  fmap f m  = map f m

instance Traversable (BBMap k) where
  traverse _ Tip = pure Tip
  traverse f (Bin s k v l r)
    = flip (Bin s k) <$> traverse f l <*> f v <*> traverse f r

instance Foldable (BBMap k) where
  foldMap _f Tip = mempty
  foldMap f (Bin _s _k v l r)
    = foldMap f l `mappend` f v `mappend` foldMap f r

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Ord k, Read k, Read e) => Read (BBMap k e) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)
#endif

{-
XXX unused code

-- parses a pair of things with the syntax a:=b
readPair :: (Read a, Read b) => ReadS (a,b)
readPair s = do (a, ct1)    <- reads s
                (":=", ct2) <- lex ct1
                (b, ct3)    <- reads ct2
                return ((a,b), ct3)
-}

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (Show k, Show a) => Show (BBMap k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format. See 'showTreeWith'.
showTree :: (Show k,Show a) => BBMap k a -> String
showTree m
  = showTreeWith showElem True False m
  where
    showElem k x  = show k ++ ":=" ++ show x


{- | /O(n)/. The expression (@'showTreeWith' showelem hang wide map@) shows
 the tree that implements the map. Elements are shown using the @showElem@ function. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

>  BBMap> let t = fromDistinctAscList [(x,()) | x <- [1..5]]
>  BBMap> putStrLn $ showTreeWith (\k x -> show (k,x)) True False t
>  (4,())
>  +--(2,())
>  |  +--(1,())
>  |  +--(3,())
>  +--(5,())
>
>  BBMap> putStrLn $ showTreeWith (\k x -> show (k,x)) True True t
>  (4,())
>  |
>  +--(2,())
>  |  |
>  |  +--(1,())
>  |  |
>  |  +--(3,())
>  |
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) False True t
>  +--(5,())
>  |
>  (4,())
>  |
>  |  +--(3,())
>  |  |
>  +--(2,())
>     |
>     +--(1,())

-}
showTreeWith :: (k -> a -> String) -> Bool -> Bool -> BBMap k a -> String
showTreeWith showelem hang wide t
  | hang      = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""

showsTree :: (k -> a -> String) -> Bool -> [String] -> [String] -> BBMap k a -> ShowS
showsTree showelem wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Bin _ kx x Tip Tip
          -> showsBars lbars . showString (showelem kx x) . showString "\n"
      Bin _ kx x l r
          -> showsTree showelem wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showelem kx x) . showString "\n" .
             showWide wide lbars .
             showsTree showelem wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: (k -> a -> String) -> Bool -> [String] -> BBMap k a -> ShowS
showsTreeHang showelem wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n"
      Bin _ kx x Tip Tip
          -> showsBars bars . showString (showelem kx x) . showString "\n"
      Bin _ kx x l r
          -> showsBars bars . showString (showelem kx x) . showString "\n" .
             showWide wide bars .
             showsTreeHang showelem wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang showelem wide (withEmpty bars) r

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

{--------------------------------------------------------------------
  Typeable
--------------------------------------------------------------------}

#include "Typeable.h"
INSTANCE_TYPEABLE2(BBMap,mapTc,"BBMap")

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal map structure is valid.
--
-- > valid (fromAscList [(3,"b"), (5,"a")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b")]) == False

valid :: Ord k => BBMap k a -> Bool
valid t = balanced t && ordered t && validsize t

ordered :: Ord a => BBMap a b -> Bool
ordered t = bounded (const True) (const True) t
  where
    bounded _  _  Tip              = True
    bounded lo hi (Bin _ kx _ l r) = (lo kx) && (hi kx) && bounded lo (<kx) l && bounded (>kx) hi r

balanced :: BBMap k a -> Bool
balanced Tip             = True
balanced (Bin _ _ _ l r) = isBalanced l r && isBalanced r l && balanced l && balanced r
{-
balanced (Bin _ _ _ l r) = balanceOK && balanced l && balanced r
  where
    balanceOK = abs (ell (size l) - ell (size r)) <= 1
    ell :: Int -> Int
    ell x = floor ((logBase 2 (fromIntegral x))::Float) + 1
-}

validsize :: BBMap a b -> Bool
validsize t = (realsize t == Just (size t))
  where
    realsize Tip = Just 0
    realsize (Bin sz _ _ l r) = case (realsize l,realsize r) of
      (Just n,Just m)  | n+m+1 == sz  -> Just sz
      _                               -> Nothing

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)
