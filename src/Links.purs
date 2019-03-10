module Links where

import Links.Bootcamp (BootcampLink)

import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)

data Link
  = Bootcamp BootcampLink

derive instance genericLink :: Generic Link _
instance eqLink :: Eq Link where
  eq = genericEq
