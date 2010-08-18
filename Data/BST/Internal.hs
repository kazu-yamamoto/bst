module Data.BST.Internal where

import Data.Bits ((.&.))

type Size    = Int
data Map k a = Tip
             | Bin !Size !k a !(Map k a) !(Map k a)
             deriving Eq

instance (Show k, Show a) => Show (Map k a) where
    show = showTree

----------------------------------------------------------------

size :: Map k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (size l + size r + 1) k x l r

----------------------------------------------------------------

(.<.) :: Size -> Size -> Bool
a .<. b
  | a >= b    = False
  | otherwise = ((a .&. b) * 2) < b

isBalanced :: Map k a -> Map k a -> Bool
isBalanced a b = not $ size a .<. ((size b) `div` 2)

----------------------------------------------------------------

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip

----------------------------------------------------------------

insert :: Ord k => k -> a -> Map k a -> Map k a
insert kx x Tip = singleton kx x
insert kx x (Bin _ ky y l r) = case compare kx ky of
    GT -> balanceGT ky y l (insert kx x r) 
    LT -> balanceLT ky y (insert kx x l) r
    EQ -> bin kx x l r

----------------------------------------------------------------

balanceGT :: k -> a -> Map k a -> Map k a -> Map k a
balanceGT k x l r
  | isBalanced l r = bin k x l r
  | otherwise      = rotateL k x l r

balanceLT :: k -> a -> Map k a -> Map k a -> Map k a
balanceLT k x l r
  | isBalanced r l = bin k x l r
  | otherwise      = rotateR k x l r

----------------------------------------------------------------
-- rotate
rotateL :: a -> b -> Map a b -> Map a b -> Map a b
rotateL k x l r@(Bin _ _ _ rl rr)
  | size rr .<. size rl = doubleL k x l r
  | otherwise           = singleL k x l r
rotateL _ _ _ _         = error "rotateL"

rotateR :: a -> b -> Map a b -> Map a b -> Map a b
rotateR k x l@(Bin _ _ _ ll lr) r
  | size ll .<. size lr = doubleR k x l r
  | otherwise           = singleR k x l r
rotateR _ _ _ _         = error "rotateR"

----------------------------------------------------------------
-- basic rotations
singleL :: a -> b -> Map a b -> Map a b -> Map a b
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ _ = error "singleL"

singleR :: a -> b -> Map a b -> Map a b -> Map a b
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ _ _ = error "singleR"

doubleL :: a -> b -> Map a b -> Map a b -> Map a b
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"

doubleR :: a -> b -> Map a b -> Map a b -> Map a b
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

----------------------------------------------------------------

valid :: Ord k => Map k a -> Bool
valid t = balanced t && ordered t && validsize t

ordered :: Ord a => Map a b -> Bool
ordered t = bounded (const True) (const True) t
  where
    bounded _  _  Tip              = True
    bounded lo hi (Bin _ kx _ l r) = (lo kx) && (hi kx) && bounded lo (<kx) l && bounded (>kx) hi r

balanced :: Map k a -> Bool
balanced Tip             = True
balanced (Bin _ _ _ l r) = isBalanced l r && isBalanced r l && balanced l && balanced r
{-
balanced (Bin _ _ _ l r) = balanceOK && balanced l && balanced r
  where
    balanceOK = abs (ell (size l) - ell (size r)) <= 1
    ell :: Int -> Int
    ell x = floor ((logBase 2 (fromIntegral x))::Float) + 1
-}

validsize :: Map a b -> Bool
validsize t = (realsize t == Just (size t))
  where
    realsize Tip = Just 0
    realsize (Bin sz _ _ l r) = case (realsize l,realsize r) of
      (Just n,Just m)  | n+m+1 == sz  -> Just sz
      _                               -> Nothing

----------------------------------------------------------------

showTree :: (Show k,Show a) => Map k a -> String
showTree m = showTreeWith showElem True False m
  where
    showElem k x  = show k ++ ":=" ++ show x

showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith showelem hang wide t
  | hang      = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""

showsTree :: (k -> a -> String) -> Bool -> [String] -> [String] -> Map k a -> ShowS
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

showsTreeHang :: (k -> a -> String) -> Bool -> [String] -> Map k a -> ShowS
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
showsBars []   = id
showsBars bars = showString (concat (reverse (tail bars))) . showString node

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars
