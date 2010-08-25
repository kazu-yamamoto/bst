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
