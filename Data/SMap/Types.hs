module Data.SMap.Types where

-- | A Map from keys @k@ to values @a@.
data Map k a  = Tip
              | Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)

type Size     = Int

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3

size :: Map k a -> Int
size t
  = case t of
      Tip             -> 0
      Bin sz _ _ _ _  -> sz


----------------------------------------------------------------

-- basic rotations
singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ Tip = error "singleL Tip"
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ Tip _ = error "singleR Tip"

doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

----------------------------------------------------------------

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r
  = Bin (size l + size r + 1) k x l r

singleton :: k -> a -> Map k a
singleton k x
  = Bin 1 k x Tip Tip
