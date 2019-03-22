module Answers.Class where

import Components.Snackbar (SnackbarContent)

import Data.Maybe (Maybe)
import Effect (Effect)


class Answerable input output
  | input -> output where
  answer :: input -> output
  report :: input -> output -> SnackbarContent

class IsChallenge input where
  showChallenge :: input -> String
  showChallengeTitle :: input -> String

class ChallengeResult input output failure
  | input -> output
  , input -> failure where
  checkChallenge :: input -> output -> Maybe failure

class RandomChallenge input where
  randomChallenge :: Effect input
