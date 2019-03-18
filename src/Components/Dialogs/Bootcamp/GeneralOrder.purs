module Components.Dialogs.Bootcamp.GeneralOrder (generalOrderDialog) where

import Answers.Bootcamp.GeneralOrders (showGeneralOrderTitle, showChallenge)
import Window.Size (WindowSize)
import Components.Dialogs.Generic (genericDialog)

import Prelude
import Data.Maybe (Maybe (..))
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue)
import IOQueues (IOQueues)
import IxSignal (IxSignal)
import Signal.Types (READ) as S
import React (ReactElement, setState, getState)
import React.DOM (text)
import React.SyntheticEvent (target)
import MaterialUI.Typography (typography)
import MaterialUI.TextField (textField')
import MaterialUI.Enums (title)
import Unsafe.Coerce (unsafeCoerce)





generalOrderDialog :: IxSignal (read :: S.READ) WindowSize
                   -> IOQueues Queue Int (Maybe String) -- ^ Write the general order index to this to open the dialog
                   -> ReactElement
generalOrderDialog = genericDialog
  { componentName: "GeneralOrderDialog"
  , titleName: "general-order-dialog-title"
  , initialState: {value: ""}
  , title: \this -> do
      {open} <- getState this
      pure
        [ text $ case open of
            Just i -> showGeneralOrderTitle i
            Nothing -> ""
        ]
  , content: \this -> do
      let changedValue e = do
            t <- target e
            setState this {state: {value: (unsafeCoerce t).value}}
      {open} <- getState this
      pure
        [ typography {gutterBottom: true, variant: title}
          [ text $ case open of
              Just i -> showChallenge i
              Nothing -> ""
          ]
        , textField' {onChange: mkEffectFn1 changedValue, fullWidth: true, multiline: true}
        ]
  , getOutput: \{value} -> value
  }
