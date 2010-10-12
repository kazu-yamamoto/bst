{-# LANGUAGE BangPatterns #-}

--module Data.RMap where
module Data.RMap (
    RMap
  , empty
  , singleton
  , insert  
  , fromList
  , toList
  ) where

import Data.RMap.Internal