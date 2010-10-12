{-# LANGUAGE BangPatterns #-}

--module Data.RMap where
module Data.RMap (
    RMap
  , empty
  , singleton
  , insert  
  , fromList
  , toList
  , lookup
  ) where

import Prelude hiding (lookup)
import Data.RMap.Internal
