module Components.Pages.Bootcamp.RankInsignias (rankInsignias) where

import Answers.Bootcamp.RankInsignias
  ( EnlistedRank, EnlistedRankInsignia
  , OfficerRank, OfficerRankInsignia
  , randomEnlistedRank
  , challengeReportEnlistedRankInsignia, challengeReportEnlistedRankAbbreviation
  , checkEnlistedRankInsignia, checkEnlistedRankAbbreviation
  , indexToEnlistedRank, enlistedRankToIndex, showEnlistedRankFull
  , randomOfficerRank
  , challengeReportOfficerRankInsignia, challengeReportOfficerRankAbbreviation
  , checkOfficerRankInsignia, checkOfficerRankAbbreviation
  , indexToOfficerRank, officerRankToIndex, showOfficerRankFull
  )
import Components.Pages.Scores (scores)
import Components.Snackbar (SnackbarContent)

import Prelude
import Data.Maybe (Maybe (..), fromJust)
import Data.Array (modifyAt)
import React (ReactElement, toElement)
import React.DOM (text, hr, br)
import Queue.One (Queue) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title, subheading)
import Partial.Unsafe (unsafePartial)


enlistedRankInsignias :: Q.Queue (write :: Q.WRITE) SnackbarContent
                      -> IOQueues Q.Queue EnlistedRank (Maybe EnlistedRankInsignia)
                      -> ReactElement
enlistedRankInsignias = scores
  { componentName: "EnlistedRankInsignias"
  , randomButtonText: "Random Enlisted Rank"
  , randomInput: randomEnlistedRank
  , checkChallenge: \r o scores ->
    let go x@{success,failure} = case checkEnlistedRankInsignia r o of
          Nothing -> x {success = success + 1}
          Just _ -> x {failure = failure + 1}
    in  unsafePartial $ fromJust $ modifyAt (enlistedRankToIndex r) go scores
  , challengeReport: challengeReportEnlistedRankInsignia
  , buttonText: showEnlistedRankFull
  , indexToInput: \i -> unsafePartial $ fromJust $ indexToEnlistedRank i
  , scoresLength: 12
  }


officerRankInsignias :: Q.Queue (write :: Q.WRITE) SnackbarContent
                      -> IOQueues Q.Queue OfficerRank (Maybe OfficerRankInsignia)
                      -> ReactElement
officerRankInsignias = scores
  { componentName: "OfficerRankInsignias"
  , randomButtonText: "Random Officer Rank"
  , randomInput: randomOfficerRank
  , checkChallenge: \r o scores ->
    let go x@{success,failure} = case checkOfficerRankInsignia r o of
          Nothing -> x {success = success + 1}
          Just _ -> x {failure = failure + 1}
    in  unsafePartial $ fromJust $ modifyAt (officerRankToIndex r) go scores
  , challengeReport: challengeReportOfficerRankInsignia
  , buttonText: showOfficerRankFull
  , indexToInput: \i -> unsafePartial $ fromJust $ indexToOfficerRank i
  , scoresLength: 10
  }


enlistedRankAbbreviations :: Q.Queue (write :: Q.WRITE) SnackbarContent
                      -> IOQueues Q.Queue EnlistedRank (Maybe String)
                      -> ReactElement
enlistedRankAbbreviations = scores
  { componentName: "EnlistedRankAbbreviations"
  , randomButtonText: "Random Enlisted Rank"
  , randomInput: randomEnlistedRank
  , checkChallenge: \r o scores ->
    let go x@{success,failure} = case checkEnlistedRankAbbreviation r o of
          Nothing -> x {success = success + 1}
          Just _ -> x {failure = failure + 1}
    in  unsafePartial $ fromJust $ modifyAt (enlistedRankToIndex r) go scores
  , challengeReport: challengeReportEnlistedRankAbbreviation
  , buttonText: showEnlistedRankFull
  , indexToInput: \i -> unsafePartial $ fromJust $ indexToEnlistedRank i
  , scoresLength: 12
  }


officerRankAbbreviations :: Q.Queue (write :: Q.WRITE) SnackbarContent
                      -> IOQueues Q.Queue OfficerRank (Maybe String)
                      -> ReactElement
officerRankAbbreviations = scores
  { componentName: "OfficerRankAbbreviations"
  , randomButtonText: "Random Officer Rank"
  , randomInput: randomOfficerRank
  , checkChallenge: \r o scores ->
    let go x@{success,failure} = case checkOfficerRankAbbreviation r o of
          Nothing -> x {success = success + 1}
          Just _ -> x {failure = failure + 1}
    in  unsafePartial $ fromJust $ modifyAt (officerRankToIndex r) go scores
  , challengeReport: challengeReportOfficerRankAbbreviation
  , buttonText: showOfficerRankFull
  , indexToInput: \i -> unsafePartial $ fromJust $ indexToOfficerRank i
  , scoresLength: 10
  }


rankInsignias :: Q.Queue (write :: Q.WRITE) SnackbarContent
              -> { enlisted ::
                   { insignia :: IOQueues Q.Queue EnlistedRank (Maybe EnlistedRankInsignia)
                   , abbreviation :: IOQueues Q.Queue EnlistedRank (Maybe String)
                   }
                 , officer ::
                   { insignia :: IOQueues Q.Queue OfficerRank (Maybe OfficerRankInsignia)
                   , abbreviation :: IOQueues Q.Queue OfficerRank (Maybe String)
                   }
                 }
              -> ReactElement
rankInsignias snackbarQueue dialogQueues = toElement
  [ typography {gutterBottom: true, variant: title} [text "Ranks"]
  , hr []
  , typography {gutterBottom: true, variant: subheading} [text "Insignias"]
  , hr []
  , br []
  , enlistedRankInsignias snackbarQueue dialogQueues.enlisted.insignia
  , br []
  , officerRankInsignias snackbarQueue dialogQueues.officer.insignia
  , br []
  , typography {gutterBottom: true, variant: subheading} [text "Abbreviations"]
  , hr []
  , br []
  , enlistedRankAbbreviations snackbarQueue dialogQueues.enlisted.abbreviation
  , br []
  , officerRankAbbreviations snackbarQueue dialogQueues.officer.abbreviation
  ]

