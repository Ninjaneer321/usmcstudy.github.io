module Components.Pages.Bootcamp.Leadership where

import Answers.Bootcamp.Leadership
  ( randomPrincipalIndex, challengeReportPrincipals, checkChallengePrincipals
  , challengeReportTraits, checkChallengeTraits
  )
import Components.Pages.Scores (scores)
import Components.Snackbar (SnackbarContent)

import Prelude
import Data.Maybe (Maybe (..), fromJust)
import Data.Array (modifyAt)
import Data.Int.Prose (proseInt)
import Data.Set (Set)
import React (ReactElement, toElement)
import React.DOM (text, hr, br)
import Queue.One (Queue) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title, subheading)
import Partial.Unsafe (unsafePartial)



leadershipTraits :: Q.Queue (write :: Q.WRITE) SnackbarContent
                 -> IOQueues Q.Queue Unit (Maybe (Set String))
                 -> ReactElement
leadershipTraits = scores
  { componentName: "LeadershipTraits"
  , random: Nothing
  , checkChallenge: \_ ts scores ->
    let go x@{success,failure} = case checkChallengeTraits ts of
          Nothing -> x {success = success + 1}
          Just _  -> x {failure = failure + 1}
    in  unsafePartial $ fromJust $ modifyAt 0 go scores
  , challengeReport: \_ -> challengeReportTraits
  , buttonText: \_ -> "Traits"
  , indexToInput: \_ -> unit
  , scoresLength: 1
  }




leadershipPrincipals :: Q.Queue (write :: Q.WRITE) SnackbarContent
                     -> IOQueues Q.Queue Int (Maybe String)
                     -> ReactElement
leadershipPrincipals = scores
  { componentName: "LeadershipPrincipals"
  , random: Just
    { randomButtonText: "Random"
    , randomInput: randomPrincipalIndex
    }
  , checkChallenge: \i s scores ->
    let go x@{success,failure} = case checkChallengePrincipals i s of
          Nothing -> x {success = success + 1}
          Just _  -> x {failure = failure + 1}
    in  unsafePartial $ fromJust $ modifyAt (i - 1) go scores
  , challengeReport: challengeReportPrincipals
  , buttonText: proseInt true
  , indexToInput: \i -> i + 1
  , scoresLength: 11
  }



leadership :: Q.Queue (write :: Q.WRITE) SnackbarContent
           -> { traits :: IOQueues Q.Queue Unit (Maybe (Set String))
              , principals :: IOQueues Q.Queue Int (Maybe String)
              }
           -> ReactElement
leadership snackbarQueue dialogQueues = toElement
  [ typography {gutterBottom: true, variant: title} [text "Leadership"]
  , hr []
  , br []
  , leadershipTraits snackbarQueue dialogQueues.traits
  , br []
  , typography {gutterBottom: true, variant: subheading} [text "Principals"]
  , hr []
  , br []
  , leadershipPrincipals snackbarQueue dialogQueues.principals
  ]
