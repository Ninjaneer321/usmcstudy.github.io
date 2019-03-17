module Answers.Bootcamp.Leadership where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Set (Set, fromFoldable, difference, isEmpty)
import Data.String.Yarn (words)
import Data.Array.Diff (diffArray)
import Data.Int.Prose (proseInt)
import Partial.Unsafe (unsafePartial)



leadershipTraits :: Set String
leadershipTraits = fromFoldable
  [ "Justice"
  , "Judgment"
  , "Decisiveness"
  , "Integrity"
  , "Dependability"
  , "Tact"
  , "Initiative"
  , "Endurance"
  , "Bearing"
  , "Unselfishness"
  , "Courage"
  , "Knowledge"
  , "Loyalty"
  , "Enthusiasm"
  ]


leadershipPrincipals :: Int -> String
leadershipPrincipals i = unsafePartial $ case i of
  1 -> "Be technically and tactically proficient."
  2 -> "Know yourself and seek self-improvement."
  3 -> "Know your Marines and look out for their welfare."
  4 -> "Keep your Marines informed."
  5 -> "Set the example."
  6 -> "Ensure that the task is understood, supervised and accomplished."
  7 -> "Train your Marines as a team."
  8 -> "Make sound and timely decisions."
  9 -> "Develop a sense of responsibility in your subordinates."
  10 -> "Employ your command in accordance with its capabilities."
  11 -> "Take responsibility for your actions."


showChallengeTraits :: String
showChallengeTraits = "What are the Leadership Traits of the Marine Corps?"

showChallengePrincipals :: Int -> String
showChallengePrincipals i =
  "what is the " <> proseInt false i <> " Leadership Principal?"



checkChallengeTraits :: Set String
                     -> Maybe {missing :: Maybe (Set String), extra :: Maybe (Set String)}
checkChallengeTraits cs =
  if cs == leadershipTraits
    then Nothing
    else
      let missing =
            let q = leadershipTraits `difference` cs
            in  if isEmpty q
                  then Nothing
                  else Just q
          extra =
            let q = cs `difference` leadershipTraits
            in  if isEmpty q
                  then Nothing
                  else Just q
      in  Just {missing, extra}


checkChallengePrincipals :: Int -> String -> Maybe (Tuple String (Array Boolean))
checkChallengePrincipals i c =
  let v = leadershipPrincipals i
  in  if v == c
        then Nothing
        else Just $ Tuple v $ diffArray (words v) (words c)
