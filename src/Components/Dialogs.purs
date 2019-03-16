module Components.Dialogs where

import Window.Size (WindowSize)
import Answers.Bootcamp.RankInsignias (EnlistedRank, EnlistedRankInsignia)
import Components.Dialogs.Bootcamp.GeneralOrder (generalOrderDialog)
import Components.Dialogs.Bootcamp.RankInsignias (enlistedRankInsigniaDialog, enlistedRankAbbreviationDialog)

import Prelude
import Data.Maybe (Maybe)
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
    }
  }

newDialogQueues :: Effect DialogQueues
newDialogQueues = do
  generalOrderQueues <- IOQueues.new
  enlistedRankInsigniaQueues <- IOQueues.new
  enlistedRankAbbreviationQueues <- IOQueues.new
  pure
    { generalOrderQueues
    , rank:
      { enlisted:
        { insignia: enlistedRankInsigniaQueues
        , abbreviation: enlistedRankAbbreviationQueues
        }
      }
    }


dialogs :: IxSignal (read :: S.READ) WindowSize
        -> DialogQueues
        -> ReactElement
dialogs
  windowSizeSignal
  { generalOrderQueues
  , rank
  } = toElement
  [ generalOrderDialog windowSizeSignal generalOrderQueues
  , enlistedRankInsigniaDialog windowSizeSignal rank.enlisted.insignia
  , enlistedRankAbbreviationDialog windowSizeSignal rank.enlisted.abbreviation
  ]
