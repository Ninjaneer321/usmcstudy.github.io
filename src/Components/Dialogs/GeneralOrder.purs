module Components.Dialogs.GeneralOrder (generalOrderDialog) where

import Answers.GeneralOrders (showGeneralOrderTitle, showChallenge)
import Window.Size (WindowSize, isMobile)

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ) as S
import Web.File.File (File)
import Web.File.FileList (item)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, setState, getState, getProps, createLeafElement)
import React.DOM (text)
import React.SyntheticEvent (target)
import React.Queue.WhileMounted (whileMountedOne) as ReactQ
import React.Signal.WhileMounted (whileMountedIx) as ReactS
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Button (button)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.TextField (textField')
import MaterialUI.Enums (primary, title, md)
import Unsafe.Coerce (unsafeCoerce)





generalOrderDialog :: IOQueues Queue Int (Maybe String) -- ^ Write the general order index to this to open the dialog
                   -> IxSignal (read :: S.READ) WindowSize
                   -> ReactElement
generalOrderDialog (IOQueues{input,output}) windowSizeSignal = createLeafElement c {}
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

    constructor' :: ReactClassConstructor _ {index :: Maybe Int, value :: String, windowSize :: WindowSize} _
    constructor' =
      let queueOpenerHandler :: _ -> Int -> Effect Unit
          queueOpenerHandler this i = setState this {index: Just i}

          windowChangeHandler :: _ -> WindowSize -> Effect Unit
          windowChangeHandler this w = setState this {windowSize: w}

      in  ReactQ.whileMountedOne input queueOpenerHandler
            (ReactS.whileMountedIx windowSizeSignal "GeneralOrderDialog" windowChangeHandler constructor)
      where
        constructor this = do
          let close = do
                setState this {index: Nothing, value: ""}
                put output Nothing
              changedValue e = do
                t <- target e
                setState this {value: (unsafeCoerce t).value}
              submit = do
                {value} <- getState this
                setState this {index: Nothing, value: ""}
                put output (Just value)

          initWindowSize <- S.get windowSizeSignal

          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: {index: Nothing, value: "", windowSize: initWindowSize}
            , render: do
              {index, windowSize} <- getState this
              props <- getProps this
              let params open =
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , fullWidth: true
                    , fullScreen: isMobile windowSize
                    , maxWidth: md
                    , "aria-labelledby": "general-order-dialog-title"
                    }
                  dialogChildren mi =
                    [ dialogTitle {id: "general-order-dialog-title"}
                      [ text $ case mi of
                          Just i -> showGeneralOrderTitle i
                          Nothing -> ""
                      ]
                    , dialogContent_
                      [ typography {gutterBottom: true, variant: title}
                        [ text $ case mi of
                            Just i -> showChallenge i
                            Nothing -> ""
                        ]
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
              pure $ case index of
                Nothing -> dialog'' (params false) (dialogChildren Nothing)
                Just i -> dialog'' (params true) (dialogChildren (Just i))
            }


-- TODO make close status separate from index state, for cleaner transitions
