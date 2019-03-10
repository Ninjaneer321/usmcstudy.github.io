module Window.Size (WindowSize (..), windowSize, windowSizeSignal, isMobile) where

import Prelude ((<), (<$>), otherwise, bind, discard, pure, Unit, class Eq)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import IxSignal (IxSignal, make, setDiff)
import Signal.Types (READ, readOnly)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)


foreign import windowSizeImpl :: Effect Int


data WindowSize
  = XS
  | SM
  | MD
  | LG
  | XL

derive instance genericWindowSize :: Generic WindowSize _
instance eqWindowSize :: Eq WindowSize where
  eq = genericEq


isMobile :: WindowSize -> Boolean
isMobile w = case w of
  XS -> true
  SM -> true
  _ -> false



windowSize :: Effect WindowSize
windowSize = go <$> windowSizeImpl
  where
    go :: Int -> WindowSize
    go w
      | w < 600 = XS
      | w < 960 = SM
      | w < 1280 = MD
      | w < 1920 = LG
      | otherwise = XL


foreign import attachOnResizeImpl :: EffectFn1 (Effect Unit) Unit

windowSizeSignal :: Effect (IxSignal (read :: READ) WindowSize)
windowSizeSignal = do
  initSize <- windowSize
  sig <- make initSize
  runEffectFn1 attachOnResizeImpl do
    newSize <- windowSize
    setDiff newSize sig
  pure (readOnly sig)

