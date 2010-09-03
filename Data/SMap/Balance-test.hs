{-# LANGUAGE CPP #-}

module Data.SMap.Balance where

import Data.SMap.Types
import Data.Bits ((.&.), shiftR, shiftL)

#if METHOD == 1
----------------------------------------------------------------
-- Nievergelt
isBalanced :: Map k a -> Map k a -> Bool
isBalanced a b = 2 * y * y <= (x + y) * (x + y)
  where
    x = size a + 1
    y = size b + 1

isSingle :: Map k a -> Map k a -> Bool
isSingle a b = z * z < 2 * w * w
  where
    z = size a + 1
    w = size b + 1
----------------------------------------------------------------
#elif METHOD == 2
----------------------------------------------------------------
-- Roura
(.<.) :: Size -> Size -> Bool
a .<. b
  | a >= b    = False
  | otherwise = ((a .&. b) `shiftL` 1) < b

isBalanced :: Map k a -> Map k a -> Bool
isBalanced a b = not $ size a .<. (size b `shiftR` 1)

isSingle :: Map k a -> Map k a -> Bool
isSingle a b = not $ size b .<. size a
----------------------------------------------------------------
#else
----------------------------------------------------------------
-- Adams
deltaU,deltaD,ratioU,ratioD :: Int
deltaU = 20
deltaD = 10
ratioU = 10
ratioD = 10

isBalanced :: Map k a -> Map k a -> Bool
isBalanced a b = deltaU * x >= deltaD * y
  where
    x = size a + 1
    y = size b + 1

isSingle :: Map k a -> Map k a -> Bool
isSingle a b = ratioD * z < ratioU * w
  where
    z = size a + 1
    w = size b + 1
----------------------------------------------------------------
#endif

----------------------------------------------------------------
balanceL :: k -> a -> Map k a -> Map k a -> Map k a
balanceL k x l r
  | isBalanced l r = bin k x l r
  | otherwise      = rotateL k x l r

balanceR :: k -> a -> Map k a -> Map k a -> Map k a
balanceR k x l r
  | isBalanced r l = bin k x l r
  | otherwise      = rotateR k x l r

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | isBalanced l r && isBalanced r l = bin k x l r
  | size l > size r                  = rotateR k x l r
  | otherwise                        = rotateL k x l r

----------------------------------------------------------------

rotateL :: a -> b -> Map a b -> Map a b -> Map a b
rotateL k x l r@(Bin _ _ _ rl rr)
  | isSingle rl rr = singleL k x l r
  | otherwise      = doubleL k x l r
#ifdef TEST
rotateL k x l r    = singleL k x l r
#else
rotateL _ _ _ _    = error "rotateL"
#endif

rotateR :: a -> b -> Map a b -> Map a b -> Map a b
rotateR k x l@(Bin _ _ _ ll lr) r
  | isSingle lr ll = singleR k x l r
  | otherwise      = doubleR k x l r
#ifdef TEST
rotateR k x l r    = singleR k x l r
#else
rotateR _ _ _ _    = error "rotateR"
#endif

----------------------------------------------------------------

join :: Ord k => k -> a -> Map k a -> Map k a -> Map k a
join kx x Tip r  = insertMin kx x r
join kx x l Tip  = insertMax kx x l
join kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | isBalanced l r && isBalanced r l = bin kx x l r
  | isBalanced l r                   = balanceL ky y ly (join kx x ry r)
  | otherwise                        = balanceR kz z (join kx x l lz) rz

merge :: Map k a -> Map k a -> Map k a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | isBalanced l r && isBalanced r l = glue l r
  | isBalanced l r                   = balanceL kx x lx (merge rx r)
  | otherwise                        = balanceR ky y (merge l ly) ry

----------------------------------------------------------------

insertMax,insertMin :: k -> a -> Map k a -> Map k a
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

----------------------------------------------------------------

glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let ((km,m),l') = deleteFindMax l in balanceL km m l' r
  | otherwise       = let ((km,m),r') = deleteFindMin r in balanceR km m l r'

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: Map k a -> ((k,a),Map k a)
deleteFindMin t
  = case t of
      Bin _ k x Tip r -> ((k,x),r)
      Bin _ k x l r   -> let (km,l') = deleteFindMin l in (km,balance k x l' r)
      Tip             -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: Map k a -> ((k,a),Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip -> ((k,x),l)
      Bin _ k x l r   -> let (km,r') = deleteFindMax r in (km,balance k x l r')
      Tip             -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)

----------------------------------------------------------------

balanced :: Map k a -> Bool
balanced Tip             = True
balanced (Bin _ _ _ l r) = isBalanced l r && isBalanced r l && balanced l && balanced r
