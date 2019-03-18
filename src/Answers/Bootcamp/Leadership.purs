module Answers.Bootcamp.Leadership where

import Crypto.Random (randomBetween)
import Components.Snackbar (SnackbarContent, SnackbarVariant (Error), defaultSuccess)

import Prelude hiding (between)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Set (Set, fromFoldable, toUnfoldable, difference, isEmpty)
import Data.String.Yarn (words)
import Data.Array (singleton, unsafeIndex)
import Data.Array.Diff (diffArray)
import Data.Array.Extra (between)
import Data.Foldable (intercalate, fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int.Prose (proseInt)
import Effect (Effect)
import React (toElement)
import React.DOM (text, strong, br, p, span)
import React.DOM.Props (style)
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


randomPrincipalIndex :: Effect Int
randomPrincipalIndex = randomBetween 1 11


challengeReportTraits :: Set String
                      -> SnackbarContent
challengeReportTraits cs = case checkChallengeTraits cs of
  Nothing -> defaultSuccess
  Just {missing,extra} ->
    { variant: Error
    , message:
      let commasBetween xs = fold (between ", " xs)
      in  toElement $ case Tuple missing extra of
            Tuple Nothing Nothing -> [text "error - nothing"]
            Tuple (Just m) Nothing ->
              [ text "Incorrect. Missing: "
              , strong [] $ singleton $ text $ commasBetween $ toUnfoldable m
              ]
            Tuple Nothing (Just e) ->
              [ text "Incorrect. Extra: "
              , strong [] $ singleton $ text $ commasBetween $ toUnfoldable e
              ]
            Tuple (Just m) (Just e) ->
              [ text "Incorrect. Missing: "
              , strong [] $ singleton $ text $ commasBetween $ toUnfoldable m
              , br []
              , text "Extra: "
              , strong [] $ singleton $ text $ commasBetween $ toUnfoldable e
              ]
    , timeout: Nothing
    }


challengeReportPrincipals :: Int
                          -> String
                          -> SnackbarContent
challengeReportPrincipals i c = case checkChallengePrincipals i c of
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
                  spacesBetween xs = intercalate [text " "] (map singleton xs)
                  go i' isBad
                    | isBad = errorSpan $ unsafePartial $ unsafeIndex cs i'
                    | otherwise = text $ unsafePartial $ unsafeIndex cs i'
              in  spacesBetween (mapWithIndex go indicies)
            ]
    , timeout: Nothing
    }
