module Data.RMap.Internal where

--import Random hiding (split)
import System.Random.Mersenne.Pure64
--import System.Random.Mersenne
--import System.IO.Unsafe
import Data.List (foldl')
import Data.Word

data RMap k v = RMap (BST k v) Rnds

instance (Show k, Show v) => Show (RMap k v) where
    show t = showTree t

data BST k v  = Tip
              | Bin {-# UNPACK #-} !Size !k v !(BST k v) !(BST k v)
              deriving Show

{-
instance (Show k, Show v) => Show (BST k v) where
    show Tip = "Tip"
    show (Bin _ k v l r) = "Bin " ++ show k ++ " " ++ show v ++ " (" ++ show l ++ ") (" ++ show r ++ ")"
-}

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
seed :: Word64
--seed :: Word32
seed = 54321

randomSeqs :: Rnds
--randomSeqs = randoms (mkStdGen seed)
randomSeqs = iter (pureMT seed)
  where
    iter s = let (r,s') = randomInt s
             in r : iter s'
{-
randomSeqs = unsafePerformIO $ do
    s <- newMTGen (Just seed)
    randoms s
-}

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
  | root      = let !(l',r') = split k t
                    n' = size l' + size r' + 1
                in (Bin n' k v l' r',xs)
  | k == tk   = (Bin n k v l r,xs)
  | k < tk    = let !(l',xs') = insert' k v l xs
                    n' = size l' + size r + 1
                in (Bin n' tk tv l' r, xs')
  | otherwise = let !(r',xs') = insert' k v r xs
                    n' = size l + size r' + 1
                in (Bin n' tk tv l r', xs')
  where
    root = x `mod` (n + 1) == n
insert' _ _ _ _ = error "insert'"

split :: Ord k => k -> BST k v -> (BST k v,BST k v)
split _ Tip = (Tip,Tip)
split k (Bin _ tk tv l r) = case compare k tk of
  LT -> let !(lt,gt) = split k l
        in (lt,bin tk tv gt r)
  GT -> let !(lt,gt) = split k r
        in (bin tk tv l lt,gt)
  EQ -> (l,r)

fromList :: Ord k => [(k,v)] -> RMap k v
fromList kvs = foldl' (\t (k,v) -> insert k v t) empty kvs

toList :: RMap k v -> [(k,v)]
toList (RMap t _) = toList' t []
  where
    toList' Tip xs = xs
    toList' (Bin _ k x l r) xs = toList' l ((k,x) : toList' r xs)

----------------------------------------------------------------

lookup :: Ord k => k -> RMap k a -> Maybe a
lookup k t = lookup' k (bst t)

lookup' :: Ord k => k -> BST k a -> Maybe a
lookup' _ Tip = Nothing
lookup' k (Bin _ kx kv l r) = case compare k kx of
    LT -> lookup' k l
    GT -> lookup' k r
    EQ -> Just kv

----------------------------------------------------------------

valid :: Ord k => RMap k a -> Bool
valid m = ordered t && validsize t
  where
    t = bst m

valid' :: Ord k => BST k a -> Bool
valid' t = ordered t && validsize t

ordered :: Ord a => BST a b -> Bool
ordered t = bounded (const True) (const True) t
  where
    bounded _  _  Tip = True
    bounded lo hi (Bin _ kx _ l r) = (lo kx) && (hi kx) && bounded lo (<kx) l && bounded (>kx) hi r

validsize :: BST a b -> Bool
validsize t = (realsize t == Just (size t))
  where
    realsize Tip              = Just 0
    realsize (Bin sz _ _ l r) = case (realsize l,realsize r) of
        (Just n,Just m)  | n+m+1 == sz  -> Just sz
        _                               -> Nothing

----------------------------------------------------------------

showTree :: (Show k,Show a) => RMap k a -> String
showTree m
  = showTreeWith showElem True False (bst m)
  where
    showElem k x  = show k ++ ":=" ++ show x

showTreeWith :: (k -> a -> String) -> Bool -> Bool -> BST k a -> String
showTreeWith showelem hang wide t
  | hang      = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""

showsTree :: (k -> a -> String) -> Bool -> [String] -> [String] -> BST k a -> ShowS
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

showsTreeHang :: (k -> a -> String) -> Bool -> [String] -> BST k a -> ShowS
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
