module Components.Dialogs where

import Window.Size (WindowSize)
import Components.Dialogs.Bootcamp.GeneralOrder (generalOrderDialog)

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
  }

newDialogQueues :: Effect DialogQueues
newDialogQueues = do
  generalOrderQueues <- IOQueues.new
  pure
    { generalOrderQueues
    }


dialogs :: IxSignal (read :: S.READ) WindowSize
        -> DialogQueues
        -> ReactElement
dialogs windowSizeSignal {generalOrderQueues} = toElement
  [ generalOrderDialog windowSizeSignal generalOrderQueues
  ]
