module Data.Array.Extra where

import Prelude (map)
import Data.Array (singleton)
import Data.Foldable (intercalate)


between :: forall a. a -> Array a -> Array a
between sep xs = intercalate [sep] (map singleton xs)
