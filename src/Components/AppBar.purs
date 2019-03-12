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
import React.DOM (text, div, mkDOM, IsDynamic (..))
import React.DOM.Props (className, unsafeMkProps, width, height) as RP
import React.Signal.WhileMounted (whileMountedIx)
import MaterialUI.AppBar (appBar)
import MaterialUI.SvgIcon (svgIcon_)
import MaterialUI.Toolbar (toolbar_)
import MaterialUI.Button (button)
import MaterialUI.Typography (typography)
import MaterialUI.Styles (withStyles)
import MaterialUI.Enums (title, static, inherit, normal)
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
  }


indexAppBar :: IxSignal (read :: READ) WindowSize
            -> IxSignal (read :: READ) Link
            -> ReactElement
indexAppBar windowSizeSignal linkSignal = createLeafElement c' {}
  where
    c' :: ReactClass {}
    c' = withStyles styles c
      where
        c :: ReactClass {classes :: {root :: String, center :: String}}
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
                            [ svgIcon_
                              [ mkDOM (IsDynamic false) "image"
                                [ RP.unsafeMkProps "xlink:href" "./ega.svg"
                                , RP.width "24"
                                , RP.height "24"
                                ] []
                              ]
                            , typography {variant: title, color: inherit} [text "USMC Study"]
                            , div [RP.className props.classes.center]
                              [ -- button {color: inherit, onClick: mkEffectFn1 (const onNameEdit)} [text "Timeline Name"]
                              ]
                            , hrefButton (Bootcamp GeneralOrders)
                              { color: inherit
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
