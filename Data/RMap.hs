{-# LANGUAGE BangPatterns #-}

module Data.RMap (
    RMap
  , empty
  , singleton
  , insert  
  , fromList
  , toList
  ) where

--import Random hiding (split)
--import System.Random.Mersenne.Pure64
import System.Random.Mersenne
import System.IO.Unsafe
import Data.List (foldl')
import Data.Word

data RMap k v = RMap (BST k v) Rnds

instance (Show k, Show v) => Show (RMap k v) where
    show (RMap t _) = show t

data BST k v  = Tip
              | Bin {-# UNPACK #-} !Size !k v !(BST k v) !(BST k v)
                
instance (Show k, Show v) => Show (BST k v) where
    show Tip = "Tip"
    show (Bin _ k v l r) = "Bin " ++ show k ++ " " ++ show v ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

type Size     = Int
type Rnds     = [Int]

bst :: RMap k v -> BST k v
bst (RMap t _) = t

size :: BST k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz

empty :: RMap k a
empty = RMap Tip randomSeqs

singleton :: Ord k => k -> v -> RMap k v
singleton k v = RMap (Bin 1 k v Tip Tip) randomSeqs

--seed :: Int
--seed :: Word64
seed :: Word32
seed = 12345

randomSeqs :: Rnds
-- randomSeqs = randoms (mkStdGen seed)
{-
randomSeqs = iter (pureMT seed)
  where
    iter s = let (r,s') = randomInt s
             in r : iter s'
-}
randomSeqs = unsafePerformIO $ do
    s <- newMTGen (Just seed)
    randoms s

bin :: Ord k => k -> v -> BST k v -> BST k v -> BST k v
bin k v l r = Bin n k v l r
  where
    n = size l + size r + 1

insert :: Ord k => k -> v -> RMap k v -> RMap k v
insert k v (RMap t xs) = RMap t' xs'
  where
   !(t',xs') = insert' k v t xs

insert' :: Ord k => k -> v -> BST k v -> Rnds -> (BST k v,Rnds)
insert' k v Tip (_:xs) = (Bin 1 k v Tip Tip, xs)
insert' k v t@(Bin n tk tv l r) (x:xs)
  | root      = (insertRoot k v t,xs)
  | k < tk    = let !(l',xs') = insert' k v l xs
                in (Bin (n+1) tk tv l' r, xs')
  | otherwise = let !(r',xs') = insert' k v r xs
                in (Bin (n+1) tk tv l r', xs')
  where
    root = x `mod` (n + 1) == n
insert' _ _ _ _ = error "insert'"

insertRoot :: Ord k => k -> v -> BST k v -> BST k v
insertRoot k v t = Bin n k v l r
  where
    n = size t + 1
    !(l,r) = split k t

split :: Ord k => k -> BST k v -> (BST k v,BST k v)
split _ Tip = (Tip,Tip)
split k (Bin _ tk v l r) = case compare k tk of
  LT -> let !(lt,gt) = split k l
        in (lt,bin tk v gt r)
  GT -> let !(lt,gt) = split k r
        in (bin tk v l lt,gt)
  EQ -> (l,r)

fromList :: Ord k => [(k,v)] -> RMap k v
fromList kvs = foldl' (\t (k,v) -> insert k v t) empty kvs

toList :: RMap k v -> [(k,v)]
toList (RMap t _) = toList' t []
  where
    toList' Tip xs = xs
    toList' (Bin _ k x l r) xs = toList' l ((k,x) : toList' r xs)
