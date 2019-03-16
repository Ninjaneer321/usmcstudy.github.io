module Components.Dialogs.Bootcamp.RankInsignias
  ( enlistedRankInsigniaDialog
  , enlistedRankAbbreviationDialog
  ) where

import Answers.Bootcamp.RankInsignias
  ( EnlistedRank, EnlistedRankInsignia, EnlistedRankInsigniaCenter (..)
  , showChallengeEnlistedRankInsignia, showEnlistedRankInsigniaTitle, centerToIndex, indexToCenter
  , showEnlistedRankAbbreviationTitle, showChallengeEnlistedRankAbbreviation)
import Window.Size (WindowSize, isMobile)

import Prelude
import Data.Maybe (Maybe (..), fromJust)
import Data.Tuple (Tuple (..))
import Data.Array (singleton)
import Data.Int.Parse (toRadix, parseInt)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue, put)
import IOQueues (IOQueues (..))
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ) as S
import React
  ( ReactElement, ReactClass, ReactClassConstructor
  , component, setState, getState, getProps, createLeafElement)
import React.DOM (text)
import React.SyntheticEvent (target)
import React.Queue.WhileMounted (whileMountedOne) as ReactQ
import React.Signal.WhileMounted (whileMountedIx) as ReactS
import MaterialUI.Dialog (dialog'')
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogContent (dialogContent_)
import MaterialUI.DialogActions (dialogActions_)
import MaterialUI.Button (button)
import MaterialUI.Styles (withStyles)
import MaterialUI.Typography (typography)
import MaterialUI.TextField (textField', textField)
import MaterialUI.MenuItem (menuItem)
import MaterialUI.Enums (primary, title, md, normal)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)



type EnlistedInsigniaState =
  { rank :: Maybe EnlistedRank
  , chevrons :: Int
  , rockers :: Int
  , center :: Maybe EnlistedRankInsigniaCenter
  , windowSize :: WindowSize
  }

initEnlistedInsigniaState :: WindowSize -> EnlistedInsigniaState
initEnlistedInsigniaState initWindowSize =
  { rank: Nothing
  , chevrons: 0
  , rockers: 0
  , center: Nothing
  , windowSize: initWindowSize
  }



enlistedRankInsigniaDialog :: IxSignal (read :: S.READ) WindowSize
                           -> IOQueues Queue EnlistedRank (Maybe EnlistedRankInsignia)
                           -- ^ Write the general order index to this to open the dialog
                           -> ReactElement
enlistedRankInsigniaDialog windowSizeSignal (IOQueues{input,output}) = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "EnlistedRankInsigniaDialog" constructor'

    constructor' :: ReactClassConstructor _ EnlistedInsigniaState _
    constructor' =
      let queueOpenerHandler :: _ -> EnlistedRank -> Effect Unit
          queueOpenerHandler this i = setState this {rank: Just i}

          windowChangeHandler :: _ -> WindowSize -> Effect Unit
          windowChangeHandler this w = setState this {windowSize: w}

      in  ReactQ.whileMountedOne input queueOpenerHandler
            (ReactS.whileMountedIx windowSizeSignal "EnlistedRankInsigniaDialog" windowChangeHandler constructor)
      where
        constructor this = do
          let close = do
                setState this {rank: Nothing, chevrons: 0, rockers: 0, center: Nothing}
                put output Nothing
              parseInt' x = unsafePartial $ fromJust $ parseInt x (toRadix 10)
              changedChevrons e = do
                t <- target e
                setState this {chevrons: parseInt' (unsafeCoerce t).value}
              changedRockers e = do
                t <- target e
                setState this {rockers: parseInt' (unsafeCoerce t).value}
              changedCenter e = do
                t <- target e
                setState this {center: indexToCenter (unsafeCoerce t).value}
              submit = do
                {chevrons,rockers,center} <- getState this
                setState this {rank: Nothing, chevrons: 0, rockers: 0, center: Nothing}
                put output $ Just $ case Tuple chevrons (Tuple rockers center) of
                  Tuple 0 (Tuple 0 Nothing) -> Nothing
                  _ -> Just {chevrons,rockers,center}

          initWindowSize <- S.get windowSizeSignal

          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: initEnlistedInsigniaState initWindowSize
            , render: do
              {rank, chevrons, rockers, center, windowSize} <- getState this
              props <- getProps this
              let params open =
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , fullWidth: true
                    , fullScreen: isMobile windowSize
                    , maxWidth: md
                    , "aria-labelledby": "general-order-dialog-title"
                    }
                  dialogChildren mR =
                    [ dialogTitle {id: "general-order-dialog-title"}
                      [ text $ case mR of
                          Just r -> showEnlistedRankInsigniaTitle r
                          Nothing -> ""
                      ]
                    , dialogContent_
                      [ typography {gutterBottom: true, variant: title}
                        [ text $ case mR of
                            Just r -> showChallengeEnlistedRankInsignia r
                            Nothing -> ""
                        ]
                      , let params' :: {fullWidth :: Boolean}
                            params' = unsafeCoerce
                              { onChange: mkEffectFn1 changedChevrons
                              , fullWidth: true
                              , type: "number"
                              , label: "Chevrons"
                              , defaultValue: chevrons
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
                              , defaultValue: rockers
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
                              , value: centerToIndex center
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
                    , dialogActions_
                      [ button
                        { onClick: mkEffectFn1 (const close)
                        , color: primary
                        } [text "Cancel"]
                      , let params' :: {autoFocus :: Boolean}
                            params' = unsafeCoerce
                              { onClick: mkEffectFn1 (const submit)
                              , color: primary
                              , autoFocus: true
                              , type: "submit"
                              }
                        in  button params' [text "Submit"]
                      ]
                    ]
              pure $ case rank of
                Nothing -> dialog'' (params false) (dialogChildren Nothing)
                Just r -> dialog'' (params true) (dialogChildren (Just r))
            }




type EnlistedAbbreviationState =
  { rank :: Maybe EnlistedRank
  , value :: String
  , windowSize :: WindowSize
  }

initEnlistedAbbreviationState :: WindowSize -> EnlistedAbbreviationState
initEnlistedAbbreviationState initWindowSize =
  { rank: Nothing
  , value: ""
  , windowSize: initWindowSize
  }



enlistedRankAbbreviationDialog :: IxSignal (read :: S.READ) WindowSize
                               -> IOQueues Queue EnlistedRank (Maybe String)
                               -- ^ Write the general order index to this to open the dialog
                               -> ReactElement
enlistedRankAbbreviationDialog windowSizeSignal (IOQueues{input,output}) = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "EnlistedRankAbbreviationDialog" constructor'

    constructor' :: ReactClassConstructor _ EnlistedAbbreviationState _
    constructor' =
      let queueOpenerHandler :: _ -> EnlistedRank -> Effect Unit
          queueOpenerHandler this i = setState this {rank: Just i}

          windowChangeHandler :: _ -> WindowSize -> Effect Unit
          windowChangeHandler this w = setState this {windowSize: w}

      in  ReactQ.whileMountedOne input queueOpenerHandler
            (ReactS.whileMountedIx windowSizeSignal "EnlistedRankAbbreviationDialog" windowChangeHandler constructor)
      where
        constructor this = do
          let close = do
                setState this {rank: Nothing, value: ""}
                put output Nothing
              changedValue e = do
                t <- target e
                setState this {value: (unsafeCoerce t).value}
              submit = do
                {value} <- getState this
                setState this {rank: Nothing, value: ""}
                put output (Just value)

          initWindowSize <- S.get windowSizeSignal

          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: initEnlistedAbbreviationState initWindowSize
            , render: do
              {rank, value, windowSize} <- getState this
              props <- getProps this
              let params open =
                    { onClose: mkEffectFn1 (const close)
                    , open
                    , fullWidth: true
                    , fullScreen: isMobile windowSize
                    , maxWidth: md
                    , "aria-labelledby": "general-order-dialog-title"
                    }
                  dialogChildren mR =
                    [ dialogTitle {id: "general-order-dialog-title"}
                      [ text $ case mR of
                          Just r -> showEnlistedRankAbbreviationTitle r
                          Nothing -> ""
                      ]
                    , dialogContent_
                      [ typography {gutterBottom: true, variant: title}
                        [ text $ case mR of
                            Just r -> showChallengeEnlistedRankAbbreviation r
                            Nothing -> ""
                        ]
                      , let params' :: {fullWidth :: Boolean}
                            params' = unsafeCoerce
                              { onChange: mkEffectFn1 changedValue
                              , fullWidth: true
                              , margin: normal
                              , value
                              }
                        in  textField' params'
                      ]
                    , dialogActions_
                      [ button
                        { onClick: mkEffectFn1 (const close)
                        , color: primary
                        } [text "Cancel"]
                      , let params' :: {autoFocus :: Boolean}
                            params' = unsafeCoerce
                              { onClick: mkEffectFn1 (const submit)
                              , color: primary
                              , autoFocus: true
                              , type: "submit"
                              }
                        in  button params' [text "Submit"]
                      ]
                    ]
              pure $ case rank of
                Nothing -> dialog'' (params false) (dialogChildren Nothing)
                Just r -> dialog'' (params true) (dialogChildren (Just r))
            }
