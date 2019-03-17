module Answers.Bootcamp.RankInsignias where

import Crypto.Random (randomBetween)
import Components.Snackbar (SnackbarContent, SnackbarVariant (Success, Error))

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..), fromJust)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.NonEmpty (NonEmpty (..))
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import React.DOM (text)
import Partial.Unsafe (unsafePartial)


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


-- | for random generation
enlistedRankToIndex :: EnlistedRank -> Int
enlistedRankToIndex r = case r of
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

-- | for random generation
indexToEnlistedRank :: Int -> Maybe EnlistedRank
indexToEnlistedRank i = case i of
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

-- | for random generation
officerRankToIndex :: OfficerRank -> Int
officerRankToIndex r = case r of
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

-- | for random generation
indexToOfficerRank :: Int -> Maybe OfficerRank
indexToOfficerRank i = case i of
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




showEnlistedRankAbbreviation :: EnlistedRank -> String
showEnlistedRankAbbreviation r = case r of
  EPvt -> "Pvt"
  EPFC -> "PFC"
  ELCpl -> "LCpl"
  ECpl -> "Cpl"
  ESgt -> "Sgt"
  ESSgt -> "SSgt"
  EGySgt -> "GySgt"
  EMSgt -> "MSgt"
  E1stSgt -> "1stSgt"
  EMGySgt -> "MGySgt"
  ESgtMaj -> "SgtMaj"
  ESMMC -> "SMMC"

showEnlistedRankFull :: EnlistedRank -> String
showEnlistedRankFull r = case r of
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


showOfficerRankAbbreviation :: OfficerRank -> String
showOfficerRankAbbreviation r = case r of
  O2ndLt  -> "2ndLt"
  O1stLt  -> "1stLt"
  OCapt   -> "Capt"
  OMaj    -> "Maj"
  OLtCol  -> "LtCol"
  OCol    -> "Col"
  OBGen   -> "BGen"
  OMajGen -> "MajGen"
  OLtGen  -> "LtGen"
  OGen    -> "Gen"

showOfficerRankFull :: OfficerRank -> String
showOfficerRankFull r = case r of
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


enlistedRankToPayGrade :: EnlistedRank -> Int
enlistedRankToPayGrade r = case r of
  EPvt -> 1
  EPFC -> 2
  ELCpl -> 3
  ECpl -> 4
  ESgt -> 5
  ESSgt -> 6
  EGySgt -> 7
  EMSgt -> 8
  E1stSgt -> 8
  EMGySgt -> 9
  ESgtMaj -> 9
  ESMMC -> 9

officerRankToPayGrade :: OfficerRank -> Int
officerRankToPayGrade r = case r of
  O2ndLt  -> 1
  O1stLt  -> 2
  OCapt   -> 3
  OMaj    -> 4
  OLtCol  -> 5
  OCol    -> 6
  OBGen   -> 7
  OMajGen -> 8
  OLtGen  -> 9
  OGen    -> 10


payGradeToEnlistedRank :: Int -> Maybe (NonEmpty Array EnlistedRank)
payGradeToEnlistedRank r = case r of
  1 -> Just (NonEmpty EPvt [])
  2 -> Just (NonEmpty EPFC [])
  3 -> Just (NonEmpty ELCpl [])
  4 -> Just (NonEmpty ECpl [])
  5 -> Just (NonEmpty ESgt [])
  6 -> Just (NonEmpty ESSgt [])
  7 -> Just (NonEmpty EGySgt [])
  8 -> Just (NonEmpty EMSgt [E1stSgt])
  9 -> Just (NonEmpty EMGySgt [ESgtMaj, ESMMC])
  _ -> Nothing

payGradeToOfficerRank :: Int -> Maybe OfficerRank
payGradeToOfficerRank r = case r of
  1  -> Just O2ndLt
  2  -> Just O1stLt
  3  -> Just OCapt
  4  -> Just OMaj
  5  -> Just OLtCol
  6  -> Just OCol
  7  -> Just OBGen
  8  -> Just OMajGen
  9  -> Just OLtGen
  10 -> Just OGen
  _ -> Nothing




data EnlistedRankInsigniaCenter
  = CrossRifles
  | Diamond
  | BurstingBomb
  | FivePointStar
  | EagleGlobeAnchor
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




centerToIndex :: Maybe EnlistedRankInsigniaCenter -> Int
centerToIndex mc = case mc of
  Nothing -> 0
  Just c -> case c of
    CrossRifles -> 1
    Diamond -> 2
    BurstingBomb -> 3
    FivePointStar -> 4
    EagleGlobeAnchor -> 5

indexToCenter :: Int -> Maybe EnlistedRankInsigniaCenter
indexToCenter i = unsafePartial $ case i of
    0 -> Nothing
    1 -> Just CrossRifles
    2 -> Just Diamond
    3 -> Just BurstingBomb
    4 -> Just FivePointStar
    5 -> Just EagleGlobeAnchor


type EnlistedRankInsignia = Maybe {chevrons :: Int, center :: Maybe EnlistedRankInsigniaCenter, rockers :: Int}

enlistedRankToInsignia :: EnlistedRank -> EnlistedRankInsignia
enlistedRankToInsignia r = case r of
  EPvt -> Nothing
  EPFC -> Just {chevrons: 1, center: Nothing, rockers: 0}
  ELCpl -> Just {chevrons: 1, center: Just CrossRifles, rockers: 0}
  ECpl -> Just {chevrons: 2, center: Just CrossRifles, rockers: 0}
  ESgt -> Just {chevrons: 3, center: Just CrossRifles, rockers: 0}
  ESSgt -> Just {chevrons: 3, center: Just CrossRifles, rockers: 1}
  EGySgt -> Just {chevrons: 3, center: Just CrossRifles, rockers: 2}
  EMSgt -> Just {chevrons: 3, center: Just CrossRifles, rockers: 3}
  E1stSgt -> Just {chevrons: 3, center: Just Diamond, rockers: 3}
  EMGySgt -> Just {chevrons: 3, center: Just BurstingBomb, rockers: 4}
  ESgtMaj -> Just {chevrons: 3, center: Just FivePointStar, rockers: 4}
  ESMMC -> Just {chevrons: 3, center: Just EagleGlobeAnchor, rockers: 4}


data OfficerRankInsigniaColor = Gold | Silver
derive instance genericOfficerRankInsigniaColor :: Generic OfficerRankInsigniaColor _
instance eqOfficerRankInsigniaColor :: Eq OfficerRankInsigniaColor where
  eq = genericEq
instance showOfficerRankInsigniaColor :: Show OfficerRankInsigniaColor where
  show x = case x of
    Gold -> "Gold"
    Silver -> "Silver"

officerRankColorToIndex :: OfficerRankInsigniaColor -> Int
officerRankColorToIndex c = case c of
  Gold -> 0
  Silver -> 1

indexToOfficerRankColor :: Int -> OfficerRankInsigniaColor
indexToOfficerRankColor i = unsafePartial $ case i of
  0 -> Gold
  1 -> Silver


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

officerRankInsigniaToIndex :: OfficerRankInsignia -> Int
officerRankInsigniaToIndex r = case r of
  Bar _ _ -> 0
  OakLeaf _ -> 1
  Eagle -> 2
  OFivePointStar _ -> 3




officerRankToInsignia :: OfficerRank -> OfficerRankInsignia
officerRankToInsignia r = case r of
  O2ndLt  -> Bar Gold 1
  O1stLt  -> Bar Silver 1
  OCapt   -> Bar Silver 2
  OMaj    -> OakLeaf Gold
  OLtCol  -> OakLeaf Silver
  OCol    -> Eagle
  OBGen   -> OFivePointStar 1
  OMajGen -> OFivePointStar 2
  OLtGen  -> OFivePointStar 3
  OGen    -> OFivePointStar 4



-- | Returns a diff of errors, if any.
checkEnlistedRankInsignia :: EnlistedRank
                          -> EnlistedRankInsignia
                          -> Maybe
                               ( Maybe
                                 { chevrons :: Maybe Int
                                 , center :: Maybe (Maybe EnlistedRankInsigniaCenter)
                                 , rockers :: Maybe Int
                                 })
checkEnlistedRankInsignia r i =
  if i == actual
    then Nothing
    else Just $ case Tuple i actual of
      Tuple (Just i') (Just actual') -> Just
        { chevrons:
          if i'.chevrons == actual'.chevrons
            then Nothing
            else Just actual'.chevrons
        , center:
          if i'.center == actual'.center
            then Nothing
            else Just actual'.center
        , rockers:
          if i'.rockers == actual'.rockers
            then Nothing
            else Just actual'.rockers
        }
      Tuple Nothing Nothing -> Nothing -- impossible case
      Tuple (Just _) Nothing -> Nothing
      Tuple Nothing (Just actual') -> Just
        { chevrons: Just actual'.chevrons
        , center: Just actual'.center
        , rockers: Just actual'.rockers
        }
  where
    actual = enlistedRankToInsignia r


checkOfficerRankInsignia :: OfficerRank
                         -> OfficerRankInsignia
                         -> Maybe OfficerRankInsignia
checkOfficerRankInsignia r i =
  if i == actual
    then Nothing
    else Just actual
  where
    actual = officerRankToInsignia r


checkEnlistedRankAbbreviation :: EnlistedRank -> String -> Maybe String
checkEnlistedRankAbbreviation r a = if a == actual then Nothing else Just actual
  where
    actual = showEnlistedRankAbbreviation r

checkOfficerRankAbbreviation :: OfficerRank -> String -> Maybe String
checkOfficerRankAbbreviation r a = if a == actual then Nothing else Just actual
  where
    actual = showOfficerRankAbbreviation r



showChallengeEnlistedRankInsignia :: EnlistedRank -> String
showChallengeEnlistedRankInsignia r =
  "What is the Insignia for the Enlisted Rank of " <> showEnlistedRankFull r <> "?"

showChallengeEnlistedRankAbbreviation :: EnlistedRank -> String
showChallengeEnlistedRankAbbreviation r =
  "What is the Abbreviation for the Enlisted Rank of " <> showEnlistedRankFull r <> "?"

showChallengeOfficerRankInsignia :: OfficerRank -> String
showChallengeOfficerRankInsignia r =
  "What is the Insignia for the Officer Rank of " <> showOfficerRankFull r <> "?"

showChallengeOfficerRankAbbreviation :: OfficerRank -> String
showChallengeOfficerRankAbbreviation r =
  "What is the Abbreviation for the Officer Rank of " <> showOfficerRankFull r <> "?"


showEnlistedRankInsigniaTitle :: EnlistedRank -> String
showEnlistedRankInsigniaTitle r =
  "Enlisted Rank " <> showEnlistedRankFull r <> " Insignia"

showEnlistedRankAbbreviationTitle :: EnlistedRank -> String
showEnlistedRankAbbreviationTitle r =
  "Enlisted Rank " <> showEnlistedRankFull r <> " Abbreviation"

showOfficerRankInsigniaTitle :: OfficerRank -> String
showOfficerRankInsigniaTitle r =
  "Officer Rank " <> showOfficerRankFull r <> " Insignia"

showOfficerRankAbbreviationTitle :: OfficerRank -> String
showOfficerRankAbbreviationTitle r =
  "Officer Rank " <> showOfficerRankFull r <> " Abbreviation"



randomEnlistedRank :: Effect EnlistedRank
randomEnlistedRank = unsafePartial $ (fromJust <<< indexToEnlistedRank) <$> randomBetween 0 11

randomEnlistedPayGrade :: Effect Int
randomEnlistedPayGrade = randomBetween 1 9

randomOfficerRank :: Effect OfficerRank
randomOfficerRank = unsafePartial $ (fromJust <<< indexToOfficerRank) <$> randomBetween 0 9

randomOfficerPayGrade :: Effect Int
randomOfficerPayGrade = randomBetween 1 10



challengeReportEnlistedRankInsignia :: EnlistedRank
                                    -> EnlistedRankInsignia
                                    -> SnackbarContent
challengeReportEnlistedRankInsignia r i = case checkEnlistedRankInsignia r i of
  Nothing ->
    { variant: Success
    , message: text "Correct!"
    , timeout: Just (Milliseconds 2000.0)
    }
  Just mInsig ->
    { variant: Error
    , timeout: Nothing
    , message: text $ "Incorrect. " <> case mInsig of
      Nothing -> full <> " has no insignia."
      Just {chevrons,center,rockers} ->
        let cs = case chevrons of
              Nothing -> ""
              Just c' ->
                " " <> show c' <> " chevron" <> (if c' == 1 then "" else "s") <> case Tuple rockers center of
                Tuple Nothing Nothing -> "."
                _ -> ","
            rs = case rockers of
              Nothing -> ""
              Just r' -> " " <> show r' <> " rocker" <> (if r' == 1 then "" else "s") <> case center of
                Nothing -> "."
                _ -> ", and"
            c = case center of
              Nothing -> ""
              Just mC -> case mC of
                Nothing -> " no center."
                Just c' -> " " <> show c' <> " center."
        in  full <> " has" <> cs <> rs <> c
    }
    where
      full = showEnlistedRankFull r

challengeReportEnlistedRankAbbreviation :: EnlistedRank
                                        -> String
                                        -> SnackbarContent
challengeReportEnlistedRankAbbreviation r s = case checkEnlistedRankAbbreviation r s of
  Nothing ->
    { variant: Success
    , message: text "Correct!"
    , timeout: Just (Milliseconds 2000.0)
    }
  Just actual ->
    { variant: Error
    , message: text $ "Incorrect. " <> showEnlistedRankFull r <> " abbreviation is " <> actual <> "."
    , timeout: Nothing
    }

challengeReportOfficerRankInsignia :: OfficerRank
                                   -> OfficerRankInsignia
                                   -> SnackbarContent
challengeReportOfficerRankInsignia r i = case checkOfficerRankInsignia r i of
  Nothing ->
    { variant: Success
    , message: text "Correct!"
    , timeout: Just (Milliseconds 2000.0)
    }
  Just actual ->
    { variant: Error
    , timeout: Nothing
    , message: text $ "Incorrect. Officer rank " <> full <> " is " <> show actual
    }
    where
      full = showOfficerRankFull r


challengeReportOfficerRankAbbreviation :: OfficerRank
                                        -> String
                                        -> SnackbarContent
challengeReportOfficerRankAbbreviation r s = case checkOfficerRankAbbreviation r s of
  Nothing ->
    { variant: Success
    , message: text "Correct!"
    , timeout: Just (Milliseconds 2000.0)
    }
  Just actual ->
    { variant: Error
    , message: text $ "Incorrect. " <> showOfficerRankFull r <> " abbreviation is " <> actual <> "."
    , timeout: Nothing
    }


