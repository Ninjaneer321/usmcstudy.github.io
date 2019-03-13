module Components.Index where

import Components.AppBar (indexAppBar)
import Components.NavBar (navBar)
import Components.Pages (page)
import Components.Dialogs (newDialogQueues, dialogs)
import Components.Snackbar (snackbars, SnackbarContent)
import WithRoot (withRoot)
import Window.Size (WindowSize, isMobile)
import Links (Link)

import Prelude
import React (ReactElement, ReactClass, ReactClassConstructor, toElement, component, createLeafElement, getState, setState)
import React.DOM (br)
import React.Signal.WhileMounted (whileMountedIx)
import Queue.One (Queue, new) as Q
import Queue.Types (writeOnly, WRITE) as Q
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ)
import MaterialUI.Paper (paper)



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
              dialogQueues <- newDialogQueues
              ( snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
                ) <- Q.writeOnly <$> Q.new

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
                            , page linkSignal snackbarQueue dialogQueues
                            ]
                    , dialogs windowSizeSignal dialogQueues
                    , snackbars snackbarQueue
                    ]
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }
