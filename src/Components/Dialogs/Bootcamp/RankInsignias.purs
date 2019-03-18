module Components.Dialogs.Bootcamp.RankInsignias
  ( enlistedRankInsigniaDialog
  , enlistedRankAbbreviationDialog
  , officerRankInsigniaDialog
  , officerRankAbbreviationDialog
  ) where

import Answers.Bootcamp.RankInsignias
  ( EnlistedRank, EnlistedRankInsignia, EnlistedRankInsigniaCenter (..)
  , OfficerRank, OfficerRankInsignia (..), OfficerRankInsigniaColor (..)
  , showChallengeEnlistedRankInsignia, showEnlistedRankInsigniaTitle, centerToIndex, indexToCenter
  , showEnlistedRankAbbreviationTitle, showChallengeEnlistedRankAbbreviation
  , showChallengeOfficerRankInsignia, showOfficerRankInsigniaTitle
  , showOfficerRankAbbreviationTitle, showChallengeOfficerRankAbbreviation
  , officerRankColorToIndex, indexToOfficerRankColor
  )
import Window.Size (WindowSize)
import Components.Dialogs.Generic (genericDialog, intToStringDialog)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Array (singleton)
import Data.Int.Parse (toRadix, parseInt)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue)
import IOQueues (IOQueues)
import IxSignal (IxSignal)
import Signal.Types (READ) as S
import React
  ( ReactElement, setState, getState)
import React.DOM (text)
import React.SyntheticEvent (target)
import MaterialUI.Typography (typography)
import MaterialUI.TextField (textField', textField)
import MaterialUI.MenuItem (menuItem)
import MaterialUI.Enums (title, normal)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)





enlistedRankInsigniaDialog :: IxSignal (read :: S.READ) WindowSize
                           -> IOQueues Queue EnlistedRank (Maybe EnlistedRankInsignia)
                           -- ^ Write the general order index to this to open the dialog
                           -> ReactElement
enlistedRankInsigniaDialog = genericDialog
  { componentName: "EnlistedRankInsigniaDialog"
  , titleName: "enlisted-rank-insignia-dialog-title"
  , initialState:
    { chevrons: 0
    , rockers: 0
    , center: Nothing
    }
  , getOutput: \{chevrons,rockers,center} -> case Tuple chevrons (Tuple rockers center) of
      Tuple 0 (Tuple 0 Nothing) -> Nothing
      _ -> Just {chevrons,rockers,center}
  , title: \this -> do
      {open} <- getState this
      pure
        [ text $ case open of
            Just r -> showEnlistedRankInsigniaTitle r
            Nothing -> ""
        ]
  , content: \this -> do
      let changedChevrons e = do
            t <- target e
            {state} <- getState this
            case parseInt (unsafeCoerce t).value (toRadix 10) of
              Nothing -> pure unit
              Just val -> setState this {state: state {chevrons = val}}
          changedRockers e = do
            t <- target e
            {state} <- getState this
            case parseInt (unsafeCoerce t).value (toRadix 10) of
              Nothing -> pure unit
              Just val -> setState this {state: state {rockers = val}}
          changedCenter e = do
            t <- target e
            {state} <- getState this
            setState this {state: state {center = indexToCenter (unsafeCoerce t).value}}
      {open,state} <- getState this
      pure
        [ typography {gutterBottom: true, variant: title}
          [ text $ case open of
              Just r -> showChallengeEnlistedRankInsignia r
              Nothing -> ""
          ]
        , let params' :: {fullWidth :: Boolean}
              params' = unsafeCoerce
                { onChange: mkEffectFn1 changedChevrons
                , fullWidth: true
                , type: "number"
                , label: "Chevrons"
                , defaultValue: state.chevrons
                , margin: normal
                , inputProps: {min: 0, pattern: "\\d*"}
                }
          in  textField' params'
        , let params' :: {fullWidth :: Boolean}
              params' = unsafeCoerce
                { onChange: mkEffectFn1 changedRockers
                , fullWidth: true
                , type: "number"
                , label: "Rockers"
                , defaultValue: state.rockers
                , margin: normal
                , inputProps: {min: 0, pattern: "\\d*"}
                }
          in  textField' params'
        , let params' :: {fullWidth :: Boolean}
              params' = unsafeCoerce
                { onChange: mkEffectFn1 changedCenter
                , fullWidth: true
                , select: true
                , label: "Center"
                , value: centerToIndex state.center
                , margin: normal
                }
              go mx =
                let params'' :: {color :: String}
                    params'' = unsafeCoerce {key: centerToIndex mx, value: centerToIndex mx}
                in  menuItem params'' $ singleton $ text $ case mx of
                      Nothing -> "Nothing"
                      Just cent -> case cent of
                        CrossRifles -> "Cross Rifles"
                        Diamond -> "Diamond"
                        BurstingBomb -> "Bursting Bomb"
                        FivePointStar -> "Five Point Star"
                        EagleGlobeAnchor -> "Eagle, Globe and Anchor"
          in  textField params' $ map go
                [ Nothing
                , Just CrossRifles
                , Just Diamond
                , Just BurstingBomb
                , Just FivePointStar
                , Just EagleGlobeAnchor
                ]
        ]
  }





enlistedRankAbbreviationDialog :: IxSignal (read :: S.READ) WindowSize
                               -> IOQueues Queue EnlistedRank (Maybe String)
                               -- ^ Write the general order index to this to open the dialog
                               -> ReactElement
enlistedRankAbbreviationDialog = intToStringDialog
  { componentName: "EnlistedRankAbbreviationDialog"
  , titleName: "enlisted-rank-abbreviation-dialog-title"
  , title: showEnlistedRankAbbreviationTitle
  , content: showChallengeEnlistedRankAbbreviation
  }




officerRankInsigniaDialog :: IxSignal (read :: S.READ) WindowSize
                          -> IOQueues Queue OfficerRank (Maybe OfficerRankInsignia)
                          -- ^ Write the general order index to this to open the dialog
                          -> ReactElement
officerRankInsigniaDialog = genericDialog
  { componentName: "OfficerRankInsigniaDialog"
  , titleName: "officer-rank-insignia-dialog-title"
  , initialState:
    { color: Gold
    , rankIndex: 0
    , count: 0
    }
  , getOutput: \{color,rankIndex,count} ->
      unsafePartial $ case rankIndex of
        0 -> Bar color count
        1 -> OakLeaf color
        2 -> Eagle
        3 -> OFivePointStar count
  , title: \this -> do
      {open} <- getState this
      pure
        [ text $ case open of
            Just r -> showOfficerRankInsigniaTitle r
            Nothing -> ""
        ]
  , content: \this -> do
      let changedColor e = do
            t <- target e
            {state} <- getState this
            setState this {state: state {color = indexToOfficerRankColor (unsafeCoerce t).value}}
          changedRankIndex e = do
            t <- target e
            {state} <- getState this
            setState this {state: state {rankIndex = (unsafeCoerce t).value}}
          changedCount e = do
            t <- target e
            {state} <- getState this
            case parseInt (unsafeCoerce t).value (toRadix 10) of
              Nothing -> pure unit
              Just val -> setState this {state: state {count = val}}
      {open,state} <- getState this
      pure
        [ typography {gutterBottom: true, variant: title}
          [ text $ case open of
              Just r -> showChallengeOfficerRankInsignia r
              Nothing -> ""
          ]
        , let params' :: {fullWidth :: Boolean}
              params' = unsafeCoerce
                { onChange: mkEffectFn1 changedRankIndex
                , fullWidth: true
                , select: true
                , label: "Type"
                , value: state.rankIndex
                , margin: normal
                }
              go n =
                let params'' :: {color :: String}
                    params'' = unsafeCoerce {key: n, value: n}
                in  menuItem params'' $ singleton $ text $ unsafePartial $ case n of
                      0 -> "Bar"
                      1 -> "Oak Leaf"
                      2 -> "Eagle"
                      3 -> "Five Point Star"
          in  textField params' $ map go
                [ 0
                , 1
                , 2
                , 3
                ]
        , let colorInput =
                let params' :: {fullWidth :: Boolean}
                    params' = unsafeCoerce
                      { onChange: mkEffectFn1 changedColor
                      , fullWidth: true
                      , select: true
                      , label: "Color"
                      , value: officerRankColorToIndex state.color
                      , margin: normal
                      }
                    go x =
                      let params'' :: {color :: String}
                          params'' = unsafeCoerce {key: n, value: n}
                            where
                              n = officerRankColorToIndex x
                      in  menuItem params'' $ singleton $ text $ show x
                in  textField params' $ map go
                      [ Gold
                      , Silver
                      ]
          in  case state.rankIndex of
                0 -> colorInput
                1 -> colorInput
                _ -> text ""
        , let countInput =
                let params' :: {fullWidth :: Boolean}
                    params' = unsafeCoerce
                      { onChange: mkEffectFn1 changedCount
                      , fullWidth: true
                      , type: "number"
                      , label: "Count"
                      , defaultValue: state.count
                      , margin: normal
                      , inputProps: {min: 0, pattern: "\\d*"}
                      }
                in  textField' params'
          in  case state.rankIndex of
                0 -> countInput
                3 -> countInput
                _ -> text ""
        ]
  }





officerRankAbbreviationDialog :: IxSignal (read :: S.READ) WindowSize
                               -> IOQueues Queue OfficerRank (Maybe String)
                               -- ^ Write the general order index to this to open the dialog
                               -> ReactElement
officerRankAbbreviationDialog = intToStringDialog
  { componentName: "OfficerRankAbbreviationDialog"
  , titleName: "officer-rank-abbreviation-dialog-title"
  , title: showOfficerRankAbbreviationTitle
  , content: showChallengeOfficerRankAbbreviation
  }
