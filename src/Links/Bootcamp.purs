module Links.Bootcamp where

import Prelude (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)

data BootcampLink
  = GeneralOrders

derive instance genericBootcampLink :: Generic BootcampLink _
instance eqBootcampLink :: Eq BootcampLink where
  eq = genericEq
