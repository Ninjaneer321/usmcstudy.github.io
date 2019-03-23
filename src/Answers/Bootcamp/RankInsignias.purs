module Answers.Bootcamp.RankInsignias
  ( Abbreviation (..)
  , Insignia (..)
  , module Answers.Bootcamp.RankInsignias.Types
  ) where

import Answers.Bootcamp.RankInsignias.Types
  ( EnlistedRank (..), OfficerRank (..)
  , EnlistedRankInsigniaCenter (..), EnlistedRankInsignia (..), EnlistedRankInsigniaDiff (..)
  , OfficerRankInsigniaColor (..), OfficerRankInsignia (..))
import Answers.Class
  (class Answerable, class IsChallenge, class ChallengeResult, class RandomChallenge, checkChallenge, answer)
import Crypto.Random (randomBetween)
import Components.Snackbar (SnackbarVariant (Error), defaultSuccess)

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..), fromJust)
import Data.NonEmpty (NonEmpty (..))
import Data.Indexable (class FromIndex, class ToIndex, fromIndex)
import Effect (Effect)
import React.DOM (text)
import Partial.Unsafe (unsafePartial)


newtype Insignia a = Insignia a

derive newtype instance toIndexInsignia :: ToIndex a => ToIndex (Insignia a)
derive newtype instance fromIndexInsignia :: FromIndex a => FromIndex (Insignia a)
instance answerableInsigniaEnlisted :: Answerable (Insignia EnlistedRank) EnlistedRankInsignia where
  answer (Insignia r) = EnlistedRankInsignia $ case r of
    EPvt    -> {chevrons: 0, center: NoCenter, rockers: 0}
    EPFC    -> {chevrons: 1, center: NoCenter, rockers: 0}
    ELCpl   -> {chevrons: 1, center: CrossRifles, rockers: 0}
    ECpl    -> {chevrons: 2, center: CrossRifles, rockers: 0}
    ESgt    -> {chevrons: 3, center: CrossRifles, rockers: 0}
    ESSgt   -> {chevrons: 3, center: CrossRifles, rockers: 1}
    EGySgt  -> {chevrons: 3, center: CrossRifles, rockers: 2}
    EMSgt   -> {chevrons: 3, center: CrossRifles, rockers: 3}
    E1stSgt -> {chevrons: 3, center: Diamond, rockers: 3}
    EMGySgt -> {chevrons: 3, center: BurstingBomb, rockers: 4}
    ESgtMaj -> {chevrons: 3, center: FivePointStar, rockers: 4}
    ESMMC   -> {chevrons: 3, center: EagleGlobeAnchor, rockers: 4}
  report r@(Insignia r') i = case checkChallenge r i of
    Nothing -> defaultSuccess
    Just (EnlistedRankInsigniaDiff {chevrons,center,rockers}) ->
      { variant: Error
      , timeout: Nothing
      , message: text $ "Incorrect. " <> case r' of
        EPvt -> full <> " has no insignia."
        _ ->
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
                Just c' -> case c' of
                  NoCenter -> " no center."
                  _ -> " " <> show c' <> " center."
          in  full <> " has" <> cs <> rs <> c
      }
      where
        full = show r'
instance isChallengeInsigniaEnlisted :: IsChallenge (Insignia EnlistedRank) where
  showChallenge (Insignia r) =
    "What is the Insignia for the Enlisted Rank of " <> show r <> "?"
  showChallengeTitle (Insignia r) =
    "Enlisted Rank " <> show r <> " Insignia"
instance challengeResultInsigniaEnlisted :: ChallengeResult (Insignia EnlistedRank) EnlistedRankInsignia EnlistedRankInsigniaDiff where
  checkChallenge r i@(EnlistedRankInsignia i') =
    if i == actual
      then Nothing
      else Just $ EnlistedRankInsigniaDiff
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
    where
      actual@(EnlistedRankInsignia actual') = answer r
instance randomChallengeInsigniaEnlisted :: RandomChallenge (Insignia EnlistedRank) where
  randomChallenge = unsafePartial $ (Insignia <<< fromJust <<< fromIndex) <$> randomBetween 0 11
instance answerableInsigniaOfficer :: Answerable (Insignia OfficerRank) OfficerRankInsignia where
  answer (Insignia r) = case r of
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
  report r@(Insignia r') i = case checkChallenge r i of
    Nothing -> defaultSuccess
    Just actual ->
      { variant: Error
      , timeout: Nothing
      , message: text $ "Incorrect. Officer rank " <> show r' <> " is " <> show actual
      }
instance isChallengeInsigniaOfficer :: IsChallenge (Insignia OfficerRank) where
  showChallenge (Insignia r) =
    "What is the Insignia for the Officer Rank of " <> show r <> "?"
  showChallengeTitle (Insignia r) =
    "Officer Rank " <> show r <> " Insignia"
instance challengeResultInsigniaOfficer :: ChallengeResult (Insignia OfficerRank) OfficerRankInsignia OfficerRankInsignia where
  checkChallenge r i =
    if i == actual
      then Nothing
      else Just actual
    where
      actual = answer r
instance randomChallengeInsigniaOfficer :: RandomChallenge (Insignia OfficerRank) where
  randomChallenge = unsafePartial $ (Insignia <<< fromJust <<< fromIndex) <$> randomBetween 0 9



newtype Abbreviation a = Abbreviation a

derive newtype instance toIndexAbbreviation :: ToIndex a => ToIndex (Abbreviation a)
derive newtype instance fromIndexAbbreviation :: FromIndex a => FromIndex (Abbreviation a)
instance answerableAbbreviationEnlisted :: Answerable (Abbreviation EnlistedRank) String where
  answer (Abbreviation r) = case r of
    EPvt    -> "Pvt"
    EPFC    -> "PFC"
    ELCpl   -> "LCpl"
    ECpl    -> "Cpl"
    ESgt    -> "Sgt"
    ESSgt   -> "SSgt"
    EGySgt  -> "GySgt"
    EMSgt   -> "MSgt"
    E1stSgt -> "1stSgt"
    EMGySgt -> "MGySgt"
    ESgtMaj -> "SgtMaj"
    ESMMC   -> "SMMC"
  report r@(Abbreviation r') s = case checkChallenge r s of
    Nothing -> defaultSuccess
    Just actual ->
      { variant: Error
      , message: text $ "Incorrect. " <> show r' <> " abbreviation is " <> actual <> "."
      , timeout: Nothing
      }
instance isChallengeAbbreviationEnlisted :: IsChallenge (Abbreviation EnlistedRank) where
  showChallenge (Abbreviation r) =
    "What is the Abbreviation for the Enlisted Rank of " <> show r <> "?"
  showChallengeTitle (Abbreviation r) =
    "Enlisted Rank " <> show r <> " Abbreviation"
instance challengeResultAbbreviationEnlisted :: ChallengeResult (Abbreviation EnlistedRank) String String where
  checkChallenge r a = if a == actual then Nothing else Just actual
    where
      actual = answer r
instance randomChallengeAbbreviationEnlisted :: RandomChallenge (Abbreviation EnlistedRank) where
  randomChallenge = unsafePartial $ (Abbreviation <<< fromJust <<< fromIndex) <$> randomBetween 0 11
instance answerableAbbreviationOfficer :: Answerable (Abbreviation OfficerRank) String where
  answer (Abbreviation r) = case r of
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
  report r@(Abbreviation r') s = case checkChallenge r s of
    Nothing -> defaultSuccess
    Just actual ->
      { variant: Error
      , message: text $ "Incorrect. " <> show r' <> " abbreviation is " <> actual <> "."
      , timeout: Nothing
      }
instance isChallengeAbbreviationOfficer :: IsChallenge (Abbreviation OfficerRank) where
  showChallenge (Abbreviation r) =
    "What is the Abbreviation for the Officer Rank of " <> show r <> "?"
  showChallengeTitle (Abbreviation r) =
    "Officer Rank " <> show r <> " Abbreviation"
instance challengeResultAbbreviationOfficer :: ChallengeResult (Abbreviation OfficerRank) String String where
  checkChallenge r a = if a == actual then Nothing else Just actual
    where
      actual = answer r
instance randomChallengeAbbreviationOfficer :: RandomChallenge (Abbreviation OfficerRank) where
  randomChallenge = unsafePartial $ (Abbreviation <<< fromJust <<< fromIndex) <$> randomBetween 0 9



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

randomEnlistedPayGrade :: Effect Int
randomEnlistedPayGrade = randomBetween 1 9

randomOfficerPayGrade :: Effect Int
randomOfficerPayGrade = randomBetween 1 10



-- TODO make challenge for rank presence in pay grade
