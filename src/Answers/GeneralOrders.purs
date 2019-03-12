module Answers.GeneralOrders where

import Crypto.Random (randomBetween)
import Components.Snackbar (SnackbarContent, SnackbarVariant (Success, Error))
import Data.Array.Diff (diffArray)

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Array (unsafeIndex, singleton)
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Foldable (intercalate)
import Data.String.Yarn (words)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import React (toElement)
import React.DOM (text, span, p, strong)
import React.DOM.Props (style)
import Partial.Unsafe (unsafePartial)


generalOrders :: Map Int String
generalOrders = Map.fromFoldable
  [ Tuple 1 "To take charge of this post and all government property in view."
  , Tuple 2 "To walk my post in a military manner, keeping always on the alert, and observing everything that takes place within sight or hearing."
  , Tuple 3 "To report all violations of orders I am instructed to enforce."
  , Tuple 4 "To repeat all calls from posts more distant from the guard house than my own."
  , Tuple 5 "To quit my post only when properly relieved."
  , Tuple 6 "To receive, obey and pass on to the sentry who relieves me, all orders from the Commanding Officer, Officer of the Day, Officers, and Non-Commissioned Officers of the guard only."
  , Tuple 7 "To talk to no one except in the line of duty."
  , Tuple 8 "To give the alarm in case of fire or disorder."
  , Tuple 9 "To call the Corporal of the Guard in any case not covered by instructions."
  , Tuple 10 "To salute all officers and all colors and standards not cased."
  , Tuple 11 "To be especially watchful at night, and, during the time for challenging, to challenge all persons on or near my post and to allow no one to pass without proper authority."
  ]



showChallenge :: Int -> String
showChallenge i =
  "What is the " <> j <> " General Order of the Marine Corps?"
  where
    j | i == 1 = "first"
      | i == 2 = "second"
      | i == 3 = "third"
      | i == 4 = "fourth"
      | i == 5 = "fifth"
      | i == 6 = "sixth"
      | i == 7 = "seventh"
      | i == 8 = "eighth"
      | i == 9 = "ninth"
      | i == 10 = "tenth"
      | i == 11 = "eleventh"
      | otherwise = ""


showGeneralOrderTitle :: Int -> String
showGeneralOrderTitle i =
  j <> " General Order"
  where
    j | i == 1 = "First"
      | i == 2 = "Second"
      | i == 3 = "Third"
      | i == 4 = "Fourth"
      | i == 5 = "Fifth"
      | i == 6 = "Sixth"
      | i == 7 = "Seventh"
      | i == 8 = "Eighth"
      | i == 9 = "Ninth"
      | i == 10 = "Tenth"
      | i == 11 = "Eleventh"
      | otherwise = ""



checkChallenge :: Int -- ^ Challenge
               -> String -- ^ Submission
               -> Maybe (Maybe (Tuple String (Array Boolean)))
checkChallenge i c = case Map.lookup i generalOrders of
  Nothing -> Nothing
  Just v
    | v == c -> Just Nothing
    | otherwise -> Just $ Just $ Tuple v $ diffArray (words v) (words c)


randomGeneralOrderIndex :: Effect Int
randomGeneralOrderIndex = randomBetween 1 11


challengeReport :: Int -- ^ Challenge
                -> String -- ^ Submission
                -> SnackbarContent
challengeReport i c = case checkChallenge i c of
  Nothing ->
    { variant: Error
    , message: text $ "Internal error - no general order with index " <> show i
    , timeout: Nothing
    }
  Just mValid -> case mValid of
    Nothing ->
      { variant: Success
      , message: text "Correct!"
      , timeout: Just (Milliseconds 1000.0)
      }
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
                    spacesBetween xs = intercalate [text " "] (map singleton xs)
                    go i' isBad
                      | isBad = errorSpan $ unsafePartial $ unsafeIndex cs i'
                      | otherwise = text $ unsafePartial $ unsafeIndex cs i'
                in  spacesBetween (mapWithIndex go indicies)
              ]
      , timeout: Nothing
      }
