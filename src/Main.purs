module Main where

import Components.Index (index)
import Window.Size (windowSizeSignal) as Window
import Links (linkSignal) as Links

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

import ReactDOM (render)
import React (ReactElement, ReactComponent, toElement)
import React.DOM (text, a)
import React.DOM.Props (href)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)


mountToRoot :: ReactElement -> Effect (Maybe ReactComponent)
mountToRoot x = do
  doc <- (toNonElementParentNode <<< toDocument) <$> (window >>= document)
  mEl <- getElementById "root" doc
  case mEl of
    Nothing -> throw "No #root <div> node!"
    Just el -> render x el


foreign import buildDate :: String


foreign import hasFlexbox :: Boolean
foreign import hasGetrandomvalues :: Boolean
foreign import hasHistory :: Boolean
foreign import hasTypedarrays :: Boolean


main :: Effect Unit
main = do
  log "Booting up application"

  log $ "Build date: " <> buildDate

  let go = do
        windowSizeSignal <- Window.windowSizeSignal
        linkSignal <- Links.linkSignal

        void $ mountToRoot $ index windowSizeSignal linkSignal

  if hasFlexbox && hasGetrandomvalues && hasHistory && hasTypedarrays
    then go
    else void $ mountToRoot $ toElement
           [ text "Unfortunately your Browser doesn't support the features required to run this app. Please upgrade, or consider using "
           , a [href "https://www.google.com/chrome/"] [text "Google Chrome"]
           , text " or "
           , a [href "https://www.mozilla.org/en-US/firefox/"] [text "Mozilla Firefox"]
           , text "."
           ]
