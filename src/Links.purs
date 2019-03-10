module Links (Link (..), linkToDocumentTitle, linkToPathname, linkToHref, pathnameToLink, linkSignal) where

import Links.Bootcamp (BootcampLink (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.String (split)
import Data.String.Pattern (Pattern (..))
import Data.Array (uncons) as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1, mkEffectFn1)
import Web.HTML (window)
import Web.HTML.Window (location, history)
import Web.HTML.Location (protocol, hostname, pathname, setPathname)
import Web.HTML.History (DocumentTitle (..), URL (..), replaceState, pushState)
import Foreign (Foreign, unsafeToForeign, unsafeFromForeign)
import IxSignal (IxSignal, make, setDiff)
import Signal.Types (READ, readOnly)
import Data.TSCompat (OptionRecord)
import Data.TSCompat.Class (class IsTSEq)
import MaterialUI.Button (button, ButtonPropsO, ButtonPropsM)
import React (ReactElement)
import React.SyntheticEvent (SyntheticMouseEvent, preventDefault)
import Record (insert) as Record
import Data.Symbol (SProxy (..))
import Prim.Row (class Lacks)
import Unsafe.Coerce (unsafeCoerce)


data Link
  = Bootcamp BootcampLink

derive instance genericLink :: Generic Link _
instance eqLink :: Eq Link where
  eq = genericEq


rootHostname :: Effect String
rootHostname = do
  l <- window >>= location
  p <- protocol l
  h <- hostname l
  pure (p <> "//" <> h)


linkToDocumentTitle :: Link -> DocumentTitle
linkToDocumentTitle l = DocumentTitle $ "USMC Study" <> case l of
  Bootcamp b -> " - Bootcamp" <> case b of
    GeneralOrders -> " - General Orders"


linkToPathname :: Link -> String
linkToPathname l = case l of
  Bootcamp b -> "/bootcamp" <> case b of
    GeneralOrders -> "/generalOrders"


linkToHref :: Link -> String
linkToHref l = host <> linkToPathname l
  where
    host = unsafePerformEffect rootHostname



-- | Either just the parsed link, or maybe the closest link requiring redirect
pathnameToLink :: String -> Either (Maybe Link) Link
pathnameToLink p = firstChunk
  where
    ps = split (Pattern "/") p
    firstChunk = case Array.uncons ps of
      Nothing -> pure (Bootcamp GeneralOrders)
      Just {head,tail}
        | head == "bootcamp" -> bootcampSecondChunk tail
        | otherwise -> Left Nothing
    bootcampSecondChunk tail = case Array.uncons tail of
      Nothing -> pure (Bootcamp GeneralOrders)
      Just {head,tail}
        | head == "generalOrders" ->
            if tail == []
              then pure (Bootcamp GeneralOrders)
              else Left (Just (Bootcamp GeneralOrders))
        | otherwise -> Left Nothing


-- | Redirects on failure to parse
getLink :: Effect Link
getLink = do
  w <- window
  l <- location w
  h <- history w
  p <- pathname l
  case pathnameToLink p of
    Right link -> pure link
    Left mLink -> do
      let link = case mLink of
            Nothing -> Bootcamp GeneralOrders
            Just link' -> link'
          path = linkToPathname link
      setPathname path l
      replaceState (unsafeToForeign link) (linkToDocumentTitle link) (URL path) h
      pure link


foreign import attachOnPopStateImpl :: EffectFn1 (EffectFn1 Foreign Unit) Unit

linkSignal :: Effect (IxSignal (read :: READ) Link)
linkSignal = do
  initLink <- getLink
  sig <- make initLink
  let go stateF = setDiff (unsafeFromForeign stateF) sig
  runEffectFn1 attachOnPopStateImpl (mkEffectFn1 go)
  pure (readOnly sig)


hrefButton :: forall a
            . IsTSEq ({ | a}) (OptionRecord (ButtonPropsO ButtonPropsM) ButtonPropsM)
           => Lacks "href" a
           => Lacks "onClick" a
           => Link
           -> { | a }
           -> Array ReactElement
           -> ReactElement
hrefButton link ps =
  let ps' :: { href :: String, onClick :: EffectFn1 SyntheticMouseEvent Unit }
      ps' = unsafeCoerce $
        let onClick = mkEffectFn1 \e -> do
              preventDefault e
              h <- window >>= history
              pushState (unsafeToForeign link) (linkToDocumentTitle link) (URL (linkToPathname link)) h
        in  Record.insert (SProxy :: SProxy "href") (linkToHref link) $
              Record.insert (SProxy :: SProxy "onClick") onClick ps
  in  button ps'


-- TODO href dropdown, href tab?
