module Components.Index where

import Components.AppBar (indexAppBar)
import Components.NavBar (navBar)
import Components.Pages (page)
import Components.Dialogs (newDialogQueues, dialogs)
import Components.Snackbar (snackbars, SnackbarContent)
import WithRoot (withRoot)
import Window.Size (WindowSize, isMobile)
import Links (Link)

import Prelude hiding (div)
import React (ReactElement, ReactClass, ReactClassConstructor, toElement, component, createLeafElement, getState, setState)
import React.DOM (br, div, text, a)
import React.DOM.Props (style, href) as RP
import React.Signal.WhileMounted (whileMountedIx)
import Queue.One (Queue, new) as Q
import Queue.Types (writeOnly, WRITE) as Q
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ)
import MaterialUI.Paper (paper)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (body1)



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
                    , div
                      [ RP.style
                        { textAlign: "center"
                        , marginTop: "10em"
                        , maxWidth: "1280px"
                        , width: "100%"
                        , marginLeft: "auto"
                        , marginRight: "auto"
                        }
                      ]
                      [ typography {variant: body1}
                        [ text "This application was developed by a poolee in the "
                        , a [RP.href "https://www.usmc.net/marines_delayed_entry/"] [text "Delayed Entry Program"]
                        , text ", from the "
                        , a [RP.href "https://www.mcrc.marines.mil/8thmcd/Units/RS-DENVER/"] [text "Metro North Recruiting Substation"]
                        , text " in Colorado."
                        ]
                      , typography {variant: body1}
                        [ text "All United States Marine Corps trademarks used in this application belong to the United States Marine Corps, including \"USMC\", the United States Marine Corps Coat of Arms, the Eagle Globe and Anchor, and color schemes, as per the "
                        , a [RP.href "https://www.hqmc.marines.mil/Portals/134/Docs/Hobbyist%20USMC%20License%20Agreement%202018.pdf?ver=2018-09-07-144222-310"] [text "Hobbyist Trademark License Agreement"]
                        , text ". Licensure is pending as of 20190313."
                        ]
                      , typography {variant: body1}
                        [ text "This application and all source code written to maintain it will be freely available for all time, under the "
                        , a [RP.href "https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html"] [text "GNU General Public License, Version 2"]
                        , text ", and can be viewed and modified through its "
                        , a [RP.href "https://github.com/usmcstudy/usmcstudy.github.io"] [text "GitHub repository"]
                        , text "."
                        ]
                      , typography {variant: body1} [text "Semper Fidelis"]
                      ]
                    , dialogs windowSizeSignal dialogQueues
                    , snackbars snackbarQueue
                    ]
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }
