module Data.Indexable where

import Data.Maybe (Maybe)


-- | Represents the ability to be converted from an Array index
class FromIndex a where
  fromIndex :: Int -> Maybe a

-- | Represents the ability to be converted to an Array index
class ToIndex a where
  toIndex :: a -> Int
