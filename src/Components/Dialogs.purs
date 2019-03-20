module Components.Dialogs where

import Window.Size (WindowSize)
import Answers.Bootcamp.RankInsignias
  ( EnlistedRank, EnlistedRankInsignia
  , OfficerRank, OfficerRankInsignia
  )
import Components.Dialogs.Bootcamp.GeneralOrder (generalOrderDialog)
import Components.Dialogs.Bootcamp.Leadership (leadershipTraitsDialog, leadershipPrincipalDialog)
import Components.Dialogs.Bootcamp.RankInsignias
  ( enlistedRankInsigniaDialog, enlistedRankAbbreviationDialog
  , officerRankInsigniaDialog, officerRankAbbreviationDialog
  )

import Prelude
import Data.Maybe (Maybe)
import Data.Set (Set)
import Effect (Effect)
import React (ReactElement, toElement)
import Queue.One (Queue) as One
import IOQueues (IOQueues)
import IOQueues (new) as IOQueues
import Signal.Types (READ) as S
import IxSignal (IxSignal)



type DialogQueues =
  { generalOrderQueues :: IOQueues One.Queue Int (Maybe String)
  , rank ::
    { enlisted ::
      { insignia :: IOQueues One.Queue EnlistedRank (Maybe EnlistedRankInsignia)
      , abbreviation :: IOQueues One.Queue EnlistedRank (Maybe String)
      }
    , officer ::
      { insignia :: IOQueues One.Queue OfficerRank (Maybe OfficerRankInsignia)
      , abbreviation :: IOQueues One.Queue OfficerRank (Maybe String)
      }
    }
  , leadership ::
    { traits :: IOQueues One.Queue Unit (Maybe (Set String))
    , principals :: IOQueues One.Queue Int (Maybe String)
    }
  }

newDialogQueues :: Effect DialogQueues
newDialogQueues = do
  generalOrderQueues <- IOQueues.new
  enlistedRankInsigniaQueues <- IOQueues.new
  enlistedRankAbbreviationQueues <- IOQueues.new
  officerRankInsigniaQueues <- IOQueues.new
  officerRankAbbreviationQueues <- IOQueues.new
  leadershipTraitsQueues <- IOQueues.new
  leadershipPrincipalsQueues <- IOQueues.new
  pure
    { generalOrderQueues
    , rank:
      { enlisted:
        { insignia: enlistedRankInsigniaQueues
        , abbreviation: enlistedRankAbbreviationQueues
        }
      , officer:
        { insignia: officerRankInsigniaQueues
        , abbreviation: officerRankAbbreviationQueues
        }
      }
    , leadership:
      { traits: leadershipTraitsQueues
      , principals: leadershipPrincipalsQueues
      }
    }


dialogs :: IxSignal (read :: S.READ) WindowSize
        -> DialogQueues
        -> ReactElement
dialogs
  windowSizeSignal
  { generalOrderQueues
  , rank
  , leadership
  } = toElement
  [ generalOrderDialog windowSizeSignal generalOrderQueues
  , enlistedRankInsigniaDialog windowSizeSignal rank.enlisted.insignia
  , enlistedRankAbbreviationDialog windowSizeSignal rank.enlisted.abbreviation
  , officerRankInsigniaDialog windowSizeSignal rank.officer.insignia
  , officerRankAbbreviationDialog windowSizeSignal rank.officer.abbreviation
  , leadershipTraitsDialog windowSizeSignal leadership.traits
  , leadershipPrincipalDialog windowSizeSignal leadership.principals
  ]
