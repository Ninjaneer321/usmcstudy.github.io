module Answers.Bootcamp.RankInsignias.Types where

import Prelude (class Eq, class Show)
import Data.Maybe (Maybe (..))
import Data.Indexable (class FromIndex, class ToIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
-- import Answers.Class ()


data EnlistedRank
  = EPvt
  | EPFC
  | ELCpl
  | ECpl
  | ESgt
  | ESSgt
  | EGySgt
  | EMSgt
  | E1stSgt
  | EMGySgt
  | ESgtMaj
  | ESMMC
derive instance genericEnlistedRank :: Generic EnlistedRank _
instance eqEnlistedRank :: Eq EnlistedRank where
  eq = genericEq
instance showEnlistedRank :: Show EnlistedRank where
  show r = case r of
    EPvt -> "Private"
    EPFC -> "Private First Class"
    ELCpl -> "Lance Corporal"
    ECpl -> "Corporal"
    ESgt -> "Sergeant"
    ESSgt -> "Staff Sergeant"
    EGySgt -> "Gunnery Sergeant"
    EMSgt -> "Master Sergeant"
    E1stSgt -> "First Sergeant"
    EMGySgt -> "Master Gunnery Sergeant"
    ESgtMaj -> "Sergeant Major"
    ESMMC -> "Sergeant Major of the Marine Corps"
instance toIndexEnlistedRank :: ToIndex EnlistedRank where
  toIndex r = case r of
    EPvt -> 0
    EPFC -> 1
    ELCpl -> 2
    ECpl -> 3
    ESgt -> 4
    ESSgt -> 5
    EGySgt -> 6
    EMSgt -> 7
    E1stSgt -> 8
    EMGySgt -> 9
    ESgtMaj -> 10
    ESMMC -> 11
instance fromIndexEnlistedRank :: FromIndex EnlistedRank where
  fromIndex i = case i of
    0 -> Just EPvt
    1 -> Just EPFC
    2 -> Just ELCpl
    3 -> Just ECpl
    4 -> Just ESgt
    5 -> Just ESSgt
    6 -> Just EGySgt
    7 -> Just EMSgt
    8 -> Just E1stSgt
    9 -> Just EMGySgt
    10 -> Just ESgtMaj
    11 -> Just ESMMC
    _ -> Nothing


data OfficerRank
  = O2ndLt
  | O1stLt
  | OCapt
  | OMaj
  | OLtCol
  | OCol
  | OBGen
  | OMajGen
  | OLtGen
  | OGen
derive instance genericOfficerRank :: Generic OfficerRank _
instance eqOfficerRank :: Eq OfficerRank where
  eq = genericEq
instance showOfficerRank :: Show OfficerRank where
  show r = case r of
    O2ndLt  -> "Second Lieutenant"
    O1stLt  -> "First Lieutenant"
    OCapt   -> "Captain"
    OMaj    -> "Major"
    OLtCol  -> "Lieutenant Colonel"
    OCol    -> "Colonel"
    OBGen   -> "Brigadier General"
    OMajGen -> "Major General"
    OLtGen  -> "Lieutenant General"
    OGen    -> "General"
instance toIndexOfficerRank :: ToIndex OfficerRank where
  toIndex r = case r of
    O2ndLt  -> 0
    O1stLt  -> 1
    OCapt   -> 2
    OMaj    -> 3
    OLtCol  -> 4
    OCol    -> 5
    OBGen   -> 6
    OMajGen -> 7
    OLtGen  -> 8
    OGen    -> 9
instance fromIndexOfficerRank :: FromIndex OfficerRank where
  fromIndex i = case i of
    0  -> Just O2ndLt
    1  -> Just O1stLt
    2  -> Just OCapt
    3  -> Just OMaj
    4  -> Just OLtCol
    5  -> Just OCol
    6  -> Just OBGen
    7  -> Just OMajGen
    8  -> Just OLtGen
    9  -> Just OGen
    _ -> Nothing



data EnlistedRankInsigniaCenter
  = CrossRifles
  | Diamond
  | BurstingBomb
  | FivePointStar
  | EagleGlobeAnchor
  | NoCenter
derive instance genericEnlistedRankInsigniaCenter :: Generic EnlistedRankInsigniaCenter _
instance eqEnlistedRankInsigniaCenter :: Eq EnlistedRankInsigniaCenter where
  eq = genericEq
instance showEnlistedRankInsigniaCenter :: Show EnlistedRankInsigniaCenter where
  show c = case c of
    CrossRifles -> "Cross Rifles"
    Diamond -> "a Diamond"
    BurstingBomb -> "a Bursting Bomb"
    FivePointStar -> "a Five Point Star"
    EagleGlobeAnchor -> "an Eagle, Globe and Anchor"
    NoCenter -> "No Center"
instance toIndexEnlistedRankInsigniaCenter :: ToIndex EnlistedRankInsigniaCenter where
  toIndex c = case c of
    NoCenter -> 0
    CrossRifles -> 1
    Diamond -> 2
    BurstingBomb -> 3
    FivePointStar -> 4
    EagleGlobeAnchor -> 5
instance fromIndexEnlistedRankInsigniaCenter :: FromIndex EnlistedRankInsigniaCenter where
  fromIndex i = case i of
    0 -> Just NoCenter
    1 -> Just CrossRifles
    2 -> Just Diamond
    3 -> Just BurstingBomb
    4 -> Just FivePointStar
    5 -> Just EagleGlobeAnchor
    _ -> Nothing



newtype EnlistedRankInsignia = EnlistedRankInsignia
  { chevrons :: Int
  , center :: EnlistedRankInsigniaCenter
  , rockers :: Int
  }
derive newtype instance eqEnlistedRankInsignia :: Eq EnlistedRankInsignia

newtype EnlistedRankInsigniaDiff = EnlistedRankInsigniaDiff
  { chevrons :: Maybe Int
  , center :: Maybe EnlistedRankInsigniaCenter
  , rockers :: Maybe Int
  }



data OfficerRankInsigniaColor = Gold | Silver
derive instance genericOfficerRankInsigniaColor :: Generic OfficerRankInsigniaColor _
instance eqOfficerRankInsigniaColor :: Eq OfficerRankInsigniaColor where
  eq = genericEq
instance showOfficerRankInsigniaColor :: Show OfficerRankInsigniaColor where
  show x = case x of
    Gold -> "Gold"
    Silver -> "Silver"
instance toIndexOfficerRankInsigniaColor :: ToIndex OfficerRankInsigniaColor where
  toIndex c = case c of
    Gold -> 0
    Silver -> 1
instance fromIndexOfficerRankInsigniaColor :: FromIndex OfficerRankInsigniaColor where
  fromIndex i = case i of
    0 -> Just Gold
    1 -> Just Silver
    _ -> Nothing


data OfficerRankInsignia
  = Bar OfficerRankInsigniaColor Int
  | OakLeaf OfficerRankInsigniaColor
  | Eagle
  | OFivePointStar Int
derive instance genericOfficerRankInsignia :: Generic OfficerRankInsignia _
instance eqOfficerRankInsignia :: Eq OfficerRankInsignia where
  eq = genericEq
instance showOfficerRankInsignia :: Show OfficerRankInsignia where
  show c = case c of
    Bar color n -> case color of
      Gold -> case n of
        1 -> "a Gold Bar"
        _ -> "error - incorrect input"
      Silver -> case n of
        1 -> "a Silver Bar"
        2 -> "Double Silver Bars"
        _ -> "error - incorrect input"
    OakLeaf color -> case color of
      Gold -> "a Gold Oak Leaf"
      Silver -> "a Silver Oak Leaf"
    Eagle -> "an Eagle"
    OFivePointStar n -> case n of
      1 -> "one Five Point Star"
      2 -> "two Five Point Stars"
      3 -> "three Five Point Stars"
      4 -> "four Five Point Stars"
      _ -> "error - incorrect input"
instance toIndexOfficerRankInsignia :: ToIndex OfficerRankInsignia where
  toIndex r = case r of
    Bar _ _ -> 0
    OakLeaf _ -> 1
    Eagle -> 2
    OFivePointStar _ -> 3
