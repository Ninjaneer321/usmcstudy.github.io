module Components.Index where

import Answers.GeneralOrders (randomGeneralOrderIndex, checkChallenge, challengeReport)
import Components.AppBar (indexAppBar)
import Components.NavBar (navBar)
import Components.Dialogs.GeneralOrder (generalOrderDialog)
-- import Components.Dialogs.Import (ImportDialog (..)) as Import
-- import Components.Dialogs.Export (exportDialog)
import Components.Snackbar (snackbars, SnackbarContent, SnackbarVariant (Success, Error))
import WithRoot (withRoot)
import Window.Size (WindowSize, isMobile)
import Links (Link)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Exception (throwException, throw)
import React (ReactElement, ReactClass, ReactClassConstructor, toElement, component, createLeafElement, getState, setState)
import React.DOM (text, br)
import React.Signal.WhileMounted (whileMountedIx)
import Queue.One (Queue, new, put) as Q
import Queue.Types (writeOnly, WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (new, callAsync) as IOQueues
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ)
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Paper (paper)
import MaterialUI.Tabs (tabs)
import MaterialUI.Enums (title)



index :: IxSignal (read :: READ) WindowSize
      -> IxSignal (read :: READ) Link
      -> ReactElement
index windowSizeSignal linkSignal = withRoot (createLeafElement c {})
  where
    c :: ReactClass {}
    c = component "Index" constructor
      where
        constructor =
          let handleWindowSize this x = setState this {windowSize: x}
          in  whileMountedIx windowSizeSignal "Index" handleWindowSize constructor'
          where
            constructor' :: ReactClassConstructor _ {windowSize :: WindowSize} _
            constructor' this = do
              ( generalOrderQueues :: IOQueues Q.Queue Int (Maybe String)
                ) <- IOQueues.new
              ( snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
                ) <- Q.writeOnly <$> Q.new

              let resolve eX = case eX of
                    Left err -> throwException err
                    Right x -> pure unit

                  generateGeneralOrder :: Effect Unit
                  generateGeneralOrder = runAff_ resolve do
                    i <- liftEffect randomGeneralOrderIndex
                    mS <- IOQueues.callAsync generalOrderQueues i
                    case mS of
                      Nothing -> pure unit
                      Just s -> liftEffect $ Q.put snackbarQueue $ challengeReport i s

              initWindowSize <- S.get windowSizeSignal
              pure
                { state: {windowSize: initWindowSize}
                , render: do
                  {windowSize} <- getState this
                  pure $ toElement
                    [ indexAppBar windowSizeSignal linkSignal
                    , let wrapper
                            | isMobile windowSize = toElement
                            | otherwise = paper
                                { style:
                                  { maxWidth: "1280px"
                                  , width: "100%"
                                  , marginLeft: "auto"
                                  , marginRight: "auto"
                                  , padding: "1em"
                                  }
                                }
                      in  wrapper
                            [ navBar linkSignal
                            , br []
                            , typography {gutterBottom: true, variant: title} [text "Eleven General Orders"]
                            , button {onClick: mkEffectFn1 (const generateGeneralOrder)} [text "Random General Order"]
                            ]
                    , generalOrderDialog generalOrderQueues windowSizeSignal
                    -- , exportDialog exportQueue
                    , snackbars snackbarQueue
                    ]
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }
