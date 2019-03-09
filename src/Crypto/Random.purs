module Crypto.Random (randomMod, randomBetween) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.ArrayBuffer.Types (ArrayView, Int32Array)
import Data.ArrayBuffer.Typed (whole, (!))
import Data.ArrayBuffer.ArrayBuffer (empty)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Exception (throw)



foreign import getRandomValuesImpl :: forall a. EffectFn1 (ArrayView a) Unit


storeRandom :: forall a. ArrayView a -> Effect Unit
storeRandom = runEffectFn1 getRandomValuesImpl

-- | Generates a random integer, modulo the input value. Note that negative randoms remain negative
randomMod :: Int -> Effect Int
randomMod v = do
  buffer <- empty 4
  (typed :: Int32Array) <- whole buffer
  storeRandom typed
  mX <- typed ! 0
  case mX of
    Nothing -> throw "No value in typed array at index 0 on random generation!"
    Just x -> pure (x `mod` v)

-- | Generates a random value between the two inputs, inclusive.
randomBetween :: Int -- ^ Low
              -> Int -- ^ High
              -> Effect Int
randomBetween bottom top =
  let v = (top - bottom) + 1
  in  (_ + bottom) <$> randomMod v
