module Answers.Bootcamp.GeneralOrders where

import Answers.Class
  (class Answerable, class IsChallenge, class ChallengeResult, class RandomChallenge, checkChallenge, answer)
import Crypto.Random (randomBetween)
import Components.Snackbar (SnackbarContent, SnackbarVariant (Error), defaultSuccess)

import Prelude hiding (between)
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Array (unsafeIndex, singleton)
import Data.Array.Diff (diffArray)
import Data.Array.Extra (between)
import Data.Int.Prose (proseInt)
import Data.String.Yarn (words)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Indexable (class FromIndex, class ToIndex)
import Effect (Effect)
import React (toElement)
import React.DOM (text, span, p, strong)
import React.DOM.Props (style)
import Partial.Unsafe (unsafePartial)



newtype GeneralOrder = GeneralOrder Int

instance toIndexGeneralOrder :: ToIndex GeneralOrder where
  toIndex (GeneralOrder i) = i - 1
instance fromIndexGeneralOrder :: FromIndex GeneralOrder where
  fromIndex i
    | i < 0 = Nothing
    | i > 10 = Nothing
    | otherwise = Just (GeneralOrder (i + 1))

instance answerableGeneralOrder :: Answerable GeneralOrder String where
  answer (GeneralOrder i) = unsafePartial $ case i of
    1 -> "To take charge of this post and all government property in view."
    2 -> "To walk my post in a military manner, keeping always on the alert, and observing everything that takes place within sight or hearing."
    3 -> "To report all violations of orders I am instructed to enforce."
    4 -> "To repeat all calls from posts more distant from the guard house than my own."
    5 -> "To quit my post only when properly relieved."
    6 -> "To receive, obey and pass on to the sentry who relieves me, all orders from the Commanding Officer, Officer of the Day, Officers, and Non-Commissioned Officers of the guard only."
    7 -> "To talk to no one except in the line of duty."
    8 -> "To give the alarm in case of fire or disorder."
    9 -> "To call the Corporal of the Guard in any case not covered by instructions."
    10 -> "To salute all officers and all colors and standards not cased."
    11 -> "To be especially watchful at night, and, during the time for challenging, to challenge all persons on or near my post and to allow no one to pass without proper authority."
  report i c = case checkChallenge i c of
    Nothing -> defaultSuccess
    Just (Tuple actual indicies) ->
      { variant: Error
      , message:
        let errorSpan x = span [style {textDecoration: "underline"}] [text x]
        in  toElement
              [ text "Incorrect. Actual: "
              , p [style {marginBottom: 0, marginTop: 0}] [strong [] [text actual]]
              , text "Submitted:"
              , p [style {marginBottom: 0, marginTop: 0}] $ singleton $ strong [] $
                let cs = words c
                    spacesBetween = between (text " ")
                    go i' isBad
                      | isBad = errorSpan $ unsafePartial $ unsafeIndex cs i'
                      | otherwise = text $ unsafePartial $ unsafeIndex cs i'
                in  spacesBetween (mapWithIndex go indicies)
              ]
      , timeout: Nothing
      }

instance isChallengeGeneralOrder :: IsChallenge GeneralOrder where
  showChallenge (GeneralOrder i) =
    "What is the " <> proseInt false i <> " General Order of the Marine Corps?"
  showChallengeTitle (GeneralOrder i) =
    proseInt true i <> " General Order"

instance challengeResultGeneralOrder :: ChallengeResult GeneralOrder String (Tuple String (Array Boolean)) where
  checkChallenge i c =
    let v = answer i
    in  if v == c
          then Nothing
          else Just $ Tuple v $ diffArray (words v) (words c)

instance randomChallengeGeneralOrder :: RandomChallenge GeneralOrder where
  randomChallenge = GeneralOrder <$> randomBetween 1 11
