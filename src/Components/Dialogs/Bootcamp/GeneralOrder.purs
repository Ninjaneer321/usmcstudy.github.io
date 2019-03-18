module Components.Dialogs.Bootcamp.GeneralOrder (generalOrderDialog) where

import Answers.Bootcamp.GeneralOrders (showGeneralOrderTitle, showChallenge)
import Window.Size (WindowSize)
import Components.Dialogs.Generic (intToStringDialog)

import Data.Maybe (Maybe)
import Queue.One (Queue)
import IOQueues (IOQueues)
import IxSignal (IxSignal)
import Signal.Types (READ) as S
import React (ReactElement)



generalOrderDialog :: IxSignal (read :: S.READ) WindowSize
                   -> IOQueues Queue Int (Maybe String) -- ^ Write the general order index to this to open the dialog
                   -> ReactElement
generalOrderDialog = intToStringDialog
  { componentName: "GeneralOrderDialog"
  , titleName: "general-order-dialog-title"
  , title: showGeneralOrderTitle
  , content: showChallenge
  }
