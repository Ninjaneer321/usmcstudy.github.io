module Components.Dialogs.GeneralOrder (generalOrderDialog) where

import Answers.GeneralOrders (showGeneralOrderTitle, showChallenge)

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import Web.File.File (File)
import Web.File.FileList (item)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, setState, getState, writeState, getProps, createLeafElement)
import React.DOM (text)
import React.SyntheticEvent (target)
import React.Queue.WhileMounted (whileMountedOne)
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Button (button)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.TextField (textField')
import MaterialUI.Enums (primary, title)
import Unsafe.Coerce (unsafeCoerce)





generalOrderDialog :: IOQueues Queue Int (Maybe String) -- ^ Write the general order index to this to open the dialog
                   -> ReactElement
generalOrderDialog (IOQueues{input,output}) = createLeafElement c {}
  where
    c :: ReactClass {}
    c = withStyles styles c'
      where
        styles :: _
        styles theme =
          {
          }

    c' :: ReactClass {classes :: {}}
    c' = component "GeneralOrderDialog" constructor'

    constructor' :: ReactClassConstructor _ {index :: Maybe Int, value :: String} _
    constructor' =
      let handler :: _ -> Int -> Effect Unit
          handler this i = setState this {index: Just i}
      in  whileMountedOne input handler constructor
      where
        constructor this =
          let close = do
                writeState this {index: Nothing, value: ""}
                put output Nothing
              changedValue e = do
                t <- target e
                setState this {value: (unsafeCoerce t).value}
              submit = do
                {value} <- getState this
                writeState this {index: Nothing, value: ""}
                put output (Just value)
          in  pure
                { componentDidMount: pure unit
                , componentWillUnmount: pure unit
                , state: {index: Nothing, value: ""}
                , render: do
                  {index} <- getState this
                  props <- getProps this
                  pure $ case index of
                    Nothing -> dialog'' {onClose: mkEffectFn1 (const close), open: false} []
                    Just i ->
                      dialog'' {onClose: mkEffectFn1 (const close), open: true, "aria-labelledby": "general-order-dialog-title"}
                        [ dialogTitle {id: "general-order-dialog-title"} [text (showGeneralOrderTitle i)]
                        , dialogContent_
                          [ typography {gutterBottom: true, variant: title} [text (showChallenge i)]
                          , textField' {onChange: mkEffectFn1 changedValue, fullWidth: true}
                          ]
                        , dialogActions_
                          [ button
                            { onClick: mkEffectFn1 (const close)
                            , color: primary
                            } [text "Cancel"]
                          , let params :: {autoFocus :: Boolean}
                                params = unsafeCoerce
                                  { onClick: mkEffectFn1 (const submit)
                                  , color: primary
                                  , autoFocus: true
                                  , type: "submit"
                                  }
                            in  button params [text "Submit"]
                          ]
                        ]
                }
