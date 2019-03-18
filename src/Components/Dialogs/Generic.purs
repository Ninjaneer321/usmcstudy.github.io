module Components.Dialogs.Generic where

import Window.Size (WindowSize, isMobile)

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ) as S
import React
  ( ReactElement, ReactClass, ReactClassConstructor, ReactThis
  , component, setState, getState, getProps, createLeafElement)
import React.DOM (text)
import React.Queue.WhileMounted (whileMountedOne) as ReactQ
import React.Signal.WhileMounted (whileMountedIx) as ReactS
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Button (button)
import MaterialUI.Enums (primary, md)
import Unsafe.Coerce (unsafeCoerce)



type State input state = {open :: Maybe input, state :: state, windowSize :: WindowSize}


genericDialog :: forall input output state
               . { componentName :: String
                 , titleName :: String
                 , initialState :: state
                 , title :: ReactThis {} (State input state) -> Effect (Array ReactElement)
                 , content :: ReactThis {} (State input state) -> Effect (Array ReactElement)
                 , getOutput :: state -> output
                 }
              -> IxSignal (read :: S.READ) WindowSize
              -> IOQueues Queue input (Maybe output)
              -> ReactElement
genericDialog
  { componentName
  , titleName
  , initialState
  , title
  , content
  , getOutput
  }
  windowSizeSignal
  (IOQueues{input,output})
  = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component componentName constructor'

    constructor' :: ReactClassConstructor {} (State input state) _
    constructor' =
      let queueOpenerHandler :: _ -> input -> Effect Unit
          queueOpenerHandler this i = setState this {open: Just i}

          windowChangeHandler :: _ -> WindowSize -> Effect Unit
          windowChangeHandler this w = setState this {windowSize: w}

      in  ReactQ.whileMountedOne input queueOpenerHandler
            (ReactS.whileMountedIx windowSizeSignal componentName windowChangeHandler constructor)
      where
        constructor this = do
          let close = do
                setState this {open: Nothing, state: initialState}
                put output Nothing
              submit = do
                {state} <- getState this
                setState this {open: Nothing, state: initialState}
                put output (Just (getOutput state))

          initWindowSize <- S.get windowSizeSignal

          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: {open: Nothing, state: initialState, windowSize: initWindowSize}
            , render: do
              {open, windowSize, state} <- getState this
              props <- getProps this
              title' <- title this
              content' <- content this
              let params isOpen =
                    { onClose: mkEffectFn1 (const close)
                    , open: isOpen
                    , fullWidth: true
                    , fullScreen: isMobile windowSize
                    , maxWidth: md
                    , "aria-labelledby": titleName
                    }
                  dialogChildren :: Array ReactElement
                  dialogChildren =
                    [ dialogTitle {id: titleName} title'
                    , dialogContent_ content'
                    , dialogActions_
                      [ button
                        { onClick: mkEffectFn1 (const close)
                        , color: primary
                        } [text "Cancel"]
                      , let params' :: {autoFocus :: Boolean}
                            params' = unsafeCoerce
                              { onClick: mkEffectFn1 (const submit)
                              , color: primary
                              , autoFocus: true
                              , type: "submit"
                              }
                        in  button params' [text "Submit"]
                      ]
                    ]
              pure $ case open of
                Nothing -> dialog'' (params false) dialogChildren
                Just i -> dialog'' (params true) dialogChildren
            }

-- TODO make close status separate from index state, for cleaner transitions
