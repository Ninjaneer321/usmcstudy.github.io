module Data.Array.Diff where

import Prelude
import Data.Maybe (Maybe (..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array ((!!))


-- | Returns true if bad, in the same length as the subject.
diffArray :: forall a
           . Eq a
          => Array a -- ^ Static, to test against
          -> Array a -- ^ Subject, to be tested
          -> Array Boolean
diffArray static subject = mapWithIndex go subject
  where
    go :: Int -> a -> Boolean
    go subjIdx subj =
      case static !! subjIdx of
        Nothing -> true
        Just stat -> subj /= stat
