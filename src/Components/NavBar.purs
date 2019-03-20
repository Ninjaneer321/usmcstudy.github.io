module Components.NavBar where

import Links (Link (..), linkToPathname, pathnameToLink, goto)
import Links.Bootcamp (BootcampLink (..))

import Prelude
import Data.Either (Either (..))
import Data.Array (singleton) as Array
import React (ReactElement, ReactClass, ReactClassConstructor, getState, setState, component, createLeafElement)
import React.SyntheticEvent (SyntheticEvent)
import React.Signal.WhileMounted (whileMountedIx)
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Effect.Exception (throw)
import Data.TSCompat (Any)
import Data.TSCompat.React (ReactNode)
import MaterialUI.Tabs (tabs)
import MaterialUI.Tab (tab')
import MaterialUI.AppBar (appBar)
import MaterialUI.Enums (secondary, scrollable, auto, static, default)
import Unsafe.Coerce (unsafeCoerce)



currentLinkNavButtons :: Link -> Array ReactElement
currentLinkNavButtons link = case link of
  Bootcamp _ ->
    [ mkTab "General Orders" (Bootcamp GeneralOrders)
    , mkTab "Ranks" (Bootcamp RankInsignias)
    , mkTab "Leadership" (Bootcamp Leadership)
    ]
  where
    mkTab name link' =
      tab'
      { label: elementToNode name
      , value: stringToValue (linkToPathname link')
      }
    elementToNode :: String -> ReactNode
    elementToNode = unsafeCoerce
    stringToValue :: String -> Any
    stringToValue = unsafeCoerce


navBar :: IxSignal (read :: READ) Link
       -> ReactElement
navBar linkSignal = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "NavBar" constructor
      where
        constructor =
          let handleLink this x = setState this {currentLink: x}
          in  whileMountedIx linkSignal "NavBar" handleLink constructor'
          where
            constructor' :: ReactClassConstructor _ {currentLink :: Link} _
            constructor' this = do
              initLink <- S.get linkSignal
              let handleValueChange :: EffectFn2 SyntheticEvent Any Unit
                  handleValueChange = mkEffectFn2 \_ x ->
                    let val = unsafeCoerce x
                    in  case pathnameToLink val of
                          Right link -> goto linkSignal link
                          Left _ -> throw $ "Couldn't parse link value " <> show val
              pure
                { state: {currentLink: initLink}
                , render: do
                  {currentLink} <- getState this
                  pure $ appBar {position: static, color: default} $ Array.singleton $ tabs
                    { value:
                      let x :: Any
                          x = unsafeCoerce (linkToPathname currentLink)
                      in  x
                    , onChange: handleValueChange
                    , indicatorColor: secondary
                    , textColor: secondary
                    , variant: scrollable
                    , scrollButtons: auto
                    } (currentLinkNavButtons currentLink)
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }
