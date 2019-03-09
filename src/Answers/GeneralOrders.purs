module Answers.GeneralOrders where

import Crypto.Random (randomBetween)

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)


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
               -> Maybe Boolean -- TODO Diff results
checkChallenge i c = case Map.lookup i generalOrders of
  Nothing -> Nothing
  Just v -> Just (c == v)


randomGeneralOrderIndex :: Effect Int
randomGeneralOrderIndex = randomBetween 1 11


challenge :: (Int -> Aff String)
          -> Aff Boolean
challenge f = do
  i <- liftEffect randomGeneralOrderIndex -- generate random general order index
  c <- f i
  case checkChallenge i c of
    Nothing -> liftEffect $ throw $ "No general order with index " <> show i
    Just valid -> pure valid
