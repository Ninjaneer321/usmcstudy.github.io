module Links
  ( Link (..), linkToDocumentTitle, linkToPathname, linkToHref
  , pathnameToLink, linkSignal, hrefButton, hrefSelect, gotoVia, goto) where

import Links.Bootcamp (BootcampLink (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.String (split)
import Data.String (uncons, drop) as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.Pattern (Pattern (..))
import Data.Array (uncons) as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, mkEffectFn1, mkEffectFn2)
import Effect.Exception (throw)
import Web.HTML (window)
import Web.HTML.Window (location, history, document)
import Web.HTML.Location (protocol, hostname, hash, setHash)
import Web.HTML.History (DocumentTitle (..), URL (..), History, replaceState, pushState)
import Web.HTML.HTMLDocument (setTitle)
import Foreign (Foreign, unsafeToForeign, unsafeFromForeign)
import IxSignal (IxSignal, make, setDiff, get)
import Signal.Types (READ, readOnly)
import Data.TSCompat (OptionRecord)
import Data.TSCompat.Class (class IsTSEq)
import Data.TSCompat.React (ReactNode)
import MaterialUI.Button (button, ButtonPropsO, ButtonPropsM)
import MaterialUI.TextField (textField)
import MaterialUI.MenuItem (menuItem)
import MaterialUI.Enums (outlined, normal)
import MaterialUI.Styles (withStyles)
import MaterialUI.Theme (Theme)
import React (ReactElement, ReactClass, ReactClassConstructor, createLeafElement, component, getState, setState, getProps)
import React.DOM (text)
import React.SyntheticEvent (SyntheticEvent, SyntheticMouseEvent, preventDefault, target)
import React.Signal.WhileMounted (whileMountedIx)
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
linkToPathname l = "#" <> case l of
  Bootcamp b -> "/bootcamp" <> case b of
    GeneralOrders -> "/generalOrders"


linkToHref :: Link -> String
linkToHref l = host <> linkToPathname l
  where
    host = unsafePerformEffect rootHostname



-- | Either just the parsed link, or maybe the closest link requiring redirect
pathnameToLink :: String -> Either (Maybe Link) Link
pathnameToLink p = case String.uncons p of
  Nothing -> firstChunk (Left [])
  Just {head,tail}
    | head == codePointFromChar '#' ->
      firstChunk $ Right $ split (Pattern "/") (String.drop 1 tail)
    | head == codePointFromChar '/' -> firstChunk $ Left $ split (Pattern "/") (String.drop 1 p)
    | otherwise -> Left Nothing
  where
    firstChunk eXs = case eXs of
      Left xs -> case go xs of
        Right x -> Left (Just x)
        Left mX -> Left mX
      Right xs -> go xs
      where
        go ys = case Array.uncons ys of
          Nothing -> pure (Bootcamp GeneralOrders)
          Just {head,tail}
            | head == "bootcamp" -> bootcampSecondChunk tail
            | otherwise -> Left Nothing
    bootcampSecondChunk xs = case Array.uncons xs of
      Nothing -> pure (Bootcamp GeneralOrders)
      Just {head,tail}
        | head == "generalOrders" ->
            if tail == []
              then pure (Bootcamp GeneralOrders)
              else Left (Just (Bootcamp GeneralOrders)) -- Redirect when there's too much, not too little
        | otherwise -> Left Nothing


gotoVia :: (Foreign -> DocumentTitle -> URL -> History -> Effect Unit)
        -> Link
        -> Effect Unit
gotoVia f link = do
  w <- window
  h <- history w
  l <- location w
  d <- document w
  let docTitle@(DocumentTitle docTitle') = linkToDocumentTitle link
      path = linkToPathname link
  f (unsafeToForeign link) docTitle (URL path) h
  setHash path l
  setTitle docTitle' d

goto :: Link -> Effect Unit
goto = gotoVia pushState



-- | Redirects on failure to parse
getLink :: Effect Link
getLink = do
  w <- window
  p <- location w >>= hash
  case pathnameToLink p of
    Right link -> do
      let (DocumentTitle docTitle') = linkToDocumentTitle link
      document w >>= setTitle docTitle'
      pure link
    Left mLink -> do
      let link = case mLink of
            Nothing -> Bootcamp GeneralOrders
            Just link' -> link'
      gotoVia replaceState link
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
              goto link
        in  Record.insert (SProxy :: SProxy "href") (linkToHref link) $
              Record.insert (SProxy :: SProxy "onClick") onClick ps
  in  button ps'


-- TODO generalize to a prism for drilling-down, external (globally unique) name, printer parser isos
hrefSelect :: forall styles
            . IxSignal (read :: READ) Link
           -> (Theme -> {textField :: { | styles }})
           -> Array Link
           -> ReactElement
hrefSelect linkSignal' styles links = createLeafElement c {}
  where
    c :: ReactClass {}
    c = withStyles styles c'
      where
        c' :: ReactClass {classes :: {textField :: String}}
        c' = component "HrefSelect" constructor'
          where
            constructor' =
              let handleLink this x = setState this {currentLink: x}
              in  whileMountedIx linkSignal' "HrefSelect" handleLink constructor
              where
                constructor :: ReactClassConstructor _ {currentLink :: Link} _
                constructor this = do
                  initLink <- get linkSignal'
                  let changed :: EffectFn2 SyntheticEvent ReactNode Unit
                      changed = mkEffectFn2 \e _ -> do
                        t <- target e
                        let val = (unsafeCoerce t).value
                        case pathnameToLink val of
                          Right link -> goto link
                          Left _ -> do
                            throw $ "Couldn't parse link value " <> show val
                  pure
                    { state: {currentLink: initLink}
                    , render: do
                        {currentLink} <- getState this
                        props <- getProps this

                        let params :: {autoFocus :: Boolean} -- to typecheck
                            params = unsafeCoerce
                              { value: linkToPathname currentLink
                              , onChange: changed
                              , select: true
                              , variant: outlined
                              , margin: normal
                              , "SelectProps": {className: props.classes.textField}
                              }
                            linkToMenuItem link =
                              let params' :: { hidden :: Boolean }
                                  params' = unsafeCoerce
                                    { value: link'
                                    , key: link'
                                    }
                                    where
                                      link' = linkToPathname link
                              in  menuItem params'
                                    [ text $ case link of
                                        Bootcamp _ -> "Bootcamp"
                                    ]
                        -- pure $ select params $ map linkToMenuItem links
                        pure $ textField params $ map linkToMenuItem links
                    , componentDidMount: pure unit
                    , componentWillUnmount: pure unit
                    }


-- TODO href dropdown, href tab?
