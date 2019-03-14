module Components.AppBar (indexAppBar) where

import Links (Link (..), hrefButton, hrefSelect)
import Links.Bootcamp (BootcampLink (GeneralOrders))
import Window.Size (WindowSize, isMobile)

import Prelude hiding (div)
import Data.Array (singleton) as Array
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, getState, setState, getProps, createLeafElement, toElement)
import React.DOM (text, div, img)
import React.DOM.Props (className, style, src) as RP
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.AppBar (appBar)
import MaterialUI.Toolbar (toolbar_)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (title, static, inherit, secondary, contained)
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ)



appButtons :: IxSignal (read :: READ) Link -> Link -> ReactElement
appButtons linkSignal currentLink = toElement
  [ hrefButton linkSignal (Bootcamp GeneralOrders)
    { color: secondary
    , variant: contained
    , disabled: case currentLink of
      Bootcamp _ -> true
      _ -> false
    } [text "Bootcamp"]
  ]



indexAppBar :: IxSignal (read :: READ) WindowSize
            -> IxSignal (read :: READ) Link
            -> ReactElement
indexAppBar windowSizeSignal linkSignal = createLeafElement c' {}
  where
    styles :: _
    styles theme =
      { root:
        { flexGrow: 1
        }
      , center:
        { flexGrow: 1
        , textAlign: "center"
        }
      , logoText:
        { marginLeft: theme.spacing.unit * 2
        }
      }
    c' :: ReactClass {}
    c' = withStyles styles c
      where
        c :: ReactClass {classes :: {root :: String, center :: String, logoText :: String}}
        c = component "IndexAppBar" constructor'
          where
            constructor' =
              let handleWindowSize this x = setState this {windowSize: x}
                  handleLink this x = setState this {currentLink: x}
              in  whileMountedIx windowSizeSignal "IndexAppBar" handleWindowSize $
                    whileMountedIx linkSignal "IndexAppBar" handleLink constructor
              where
                constructor :: ReactClassConstructor _ {windowSize :: WindowSize, currentLink :: Link} _
                constructor this = do
                  initWindowSize <- S.get windowSizeSignal
                  initLink <- S.get linkSignal
                  pure
                    { state: {windowSize: initWindowSize, currentLink: initLink}
                    , render: do
                        props <- getProps this
                        {windowSize, currentLink} <- getState this
                        pure $ appBar {position: static, className: props.classes.root} $ Array.singleton $ toolbar_ $
                          if isMobile windowSize
                            then
                              [ img [RP.src "./ega.svg", RP.style {height: "44px"}]
                              , div [RP.className props.classes.center] []
                              , hrefSelect linkSignal
                                (\theme ->
                                  { textField:
                                    { backgroundColor: theme.palette.secondary.main
                                    , color: "#fff"
                                    , borderRadius: "4px"
                                    , height: "40px"
                                    }
                                  }
                                )
                                [ Bootcamp GeneralOrders
                                ]
                              ]
                            else
                              [ img [RP.src "./ega.svg", RP.style {height: "44px"}]
                              , typography
                                { variant: title
                                , color: inherit
                                , className: props.classes.logoText
                                } [text "USMC Study"]
                              , div [RP.className props.classes.center] []
                              , appButtons linkSignal currentLink
                              ]
                    , componentDidMount: pure unit
                    , componentWillUnmount: pure unit
                    }
