module Components.AppBar (indexAppBar) where

import Links (Link (..), hrefButton)
import Links.Bootcamp (BootcampLink (GeneralOrders))
import Window.Size (WindowSize, isMobile)

import Prelude hiding (div)
import Data.TSCompat.React (ReactNode)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, getState, setState, getProps, createLeafElement)
import React.DOM (text, div, img)
import React.DOM.Props (className, style, src) as RP
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.AppBar (appBar)
import MaterialUI.SvgIcon (svgIcon_)
import MaterialUI.Toolbar (toolbar_)
import MaterialUI.Button (button)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (title, static, inherit, normal, secondary, outlined)
import Unsafe.Coerce (unsafeCoerce)
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ)



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


indexAppBar :: IxSignal (read :: READ) WindowSize
            -> IxSignal (read :: READ) Link
            -> ReactElement
indexAppBar windowSizeSignal linkSignal = createLeafElement c' {}
  where
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
                        pure $ appBar {position: static, className: props.classes.root}
                          [ toolbar_
                            [ img [RP.src "./ega.svg", RP.style {height: "44px"}]
                            , typography {variant: title, color: inherit, className: props.classes.logoText} [text "USMC Study"]
                            , div [RP.className props.classes.center]
                              [ -- button {color: inherit, onClick: mkEffectFn1 (const onNameEdit)} [text "Timeline Name"]
                              ]
                            , hrefButton (Bootcamp GeneralOrders)
                              { color: secondary
                              , variant: outlined
                              , disabled: case currentLink of
                                Bootcamp _ -> true
                                _ -> false
                              } [text "Bootcamp"]
                            -- , button {color: inherit, onClick: mkEffectFn1 (const onImport)} [text "Import"]
                            -- , button {color: inherit, onClick: mkEffectFn1 (const onExport)} [text "Export"]
                            ]
                          ]
                    , componentDidMount: pure unit
                    , componentWillUnmount: pure unit
                    }
