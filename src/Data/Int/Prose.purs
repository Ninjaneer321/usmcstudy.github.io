module Data.Int.Prose where

import Prelude
import Data.String.Yarn (capitalize)
import Partial.Unsafe (unsafePartial)


proseInt :: Boolean -> Int -> String
proseInt cap i =
  if cap
    then capitalize j
    else j
  where
    j = unsafePartial $ case i of
          1 -> "first"
          2 -> "second"
          3 -> "third"
          4 -> "fourth"
          5 -> "fifth"
          6 -> "sixth"
          7 -> "seventh"
          8 -> "eighth"
          9 -> "ninth"
          10 -> "tenth"
          11 -> "eleventh"
