module Links where

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
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (protocol, hostname)



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


linkToHref :: Link -> String
linkToHref l =
  let path = case l of
        Bootcamp b -> "/bootcamp" <> case b of
          GeneralOrders -> "/generalOrders"
  in  host <> path
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
