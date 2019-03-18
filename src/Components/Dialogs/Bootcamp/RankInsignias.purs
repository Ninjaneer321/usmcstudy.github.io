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
import Window.Size (WindowSize, isMobile)
import Components.Dialogs.Generic (genericDialog)

import Prelude
import Data.Maybe (Maybe (..))
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



-- type EnlistedInsigniaState =
--   { rank :: Maybe EnlistedRank
--   , chevrons :: Int
--   , rockers :: Int
--   , center :: Maybe EnlistedRankInsigniaCenter
--   , windowSize :: WindowSize
--   }

-- initEnlistedInsigniaState :: WindowSize -> EnlistedInsigniaState
-- initEnlistedInsigniaState initWindowSize =
--   { rank: Nothing
--   , chevrons: 0
--   , rockers: 0
--   , center: Nothing
--   , windowSize: initWindowSize
--   }



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

{-
  windowSizeSignal (IOQueues{input,output}) = createLeafElement c {}
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
              changedChevrons e = do
                t <- target e
                case parseInt (unsafeCoerce t).value (toRadix 10) of
                  Nothing -> pure unit
                  Just val -> setState this {chevrons: val}
              changedRockers e = do
                t <- target e
                case parseInt (unsafeCoerce t).value (toRadix 10) of
                  Nothing -> pure unit
                  Just val -> setState this {rockers: val}
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
-}



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



type OfficerInsigniaState =
  { rank :: Maybe OfficerRank
  , color :: OfficerRankInsigniaColor
  , rankIndex :: Int
  , count :: Int
  , windowSize :: WindowSize
  }

initOfficerInsigniaState :: WindowSize -> OfficerInsigniaState
initOfficerInsigniaState initWindowSize =
  { rank: Nothing
  , color: Gold
  , rankIndex: 0
  , count: 0
  , windowSize: initWindowSize
  }


-- TODO change to OfficerRankInsignia

officerRankInsigniaDialog :: IxSignal (read :: S.READ) WindowSize
                          -> IOQueues Queue OfficerRank (Maybe OfficerRankInsignia)
                          -- ^ Write the general order index to this to open the dialog
                          -> ReactElement
officerRankInsigniaDialog windowSizeSignal (IOQueues{input,output}) = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "OfficerRankInsigniaDialog" constructor'

    constructor' :: ReactClassConstructor _ OfficerInsigniaState _
    constructor' =
      let queueOpenerHandler :: _ -> OfficerRank -> Effect Unit
          queueOpenerHandler this i = setState this {rank: Just i}

          windowChangeHandler :: _ -> WindowSize -> Effect Unit
          windowChangeHandler this w = setState this {windowSize: w}

      in  ReactQ.whileMountedOne input queueOpenerHandler
            (ReactS.whileMountedIx windowSizeSignal "OfficerRankInsigniaDialog" windowChangeHandler constructor)
      where
        constructor this = do
          let close = do
                setState this {rank: Nothing, color: Gold, rankIndex: 0, count: 0}
                put output Nothing
              changedColor e = do
                t <- target e
                setState this {color: indexToOfficerRankColor (unsafeCoerce t).value}
              changedRankIndex e = do
                t <- target e
                setState this {rankIndex: (unsafeCoerce t).value}
              changedCount e = do
                t <- target e
                case parseInt (unsafeCoerce t).value (toRadix 10) of
                  Nothing -> pure unit
                  Just val -> setState this {count: val}
              submit = do
                {color,rankIndex,count} <- getState this
                setState this {rank: Nothing, color: Gold, rankIndex: 0, count: 0}
                put output $ Just $ unsafePartial $ case rankIndex of
                  0 -> Bar color count
                  1 -> OakLeaf color
                  2 -> Eagle
                  3 -> OFivePointStar count

          initWindowSize <- S.get windowSizeSignal

          pure
            { componentDidMount: pure unit
            , componentWillUnmount: pure unit
            , state: initOfficerInsigniaState initWindowSize
            , render: do
              {rank, color, rankIndex, count, windowSize} <- getState this
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
                          Just r -> showOfficerRankInsigniaTitle r
                          Nothing -> ""
                      ]
                    , dialogContent_
                      [ typography {gutterBottom: true, variant: title}
                        [ text $ case mR of
                            Just r -> showChallengeOfficerRankInsignia r
                            Nothing -> ""
                        ]
                      , let params' :: {fullWidth :: Boolean}
                            params' = unsafeCoerce
                              { onChange: mkEffectFn1 changedRankIndex
                              , fullWidth: true
                              , select: true
                              , label: "Type"
                              , value: rankIndex
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
                                    , value: officerRankColorToIndex color
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
                        in  case rankIndex of
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
                                    , defaultValue: count
                                    , margin: normal
                                    , inputProps: {min: 0, pattern: "\\d*"}
                                    }
                              in  textField' params'
                        in  case rankIndex of
                              0 -> countInput
                              3 -> countInput
                              _ -> text ""
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



type OfficerAbbreviationState =
  { rank :: Maybe OfficerRank
  , value :: String
  , windowSize :: WindowSize
  }

initOfficerAbbreviationState :: WindowSize -> OfficerAbbreviationState
initOfficerAbbreviationState initWindowSize =
  { rank: Nothing
  , value: ""
  , windowSize: initWindowSize
  }



officerRankAbbreviationDialog :: IxSignal (read :: S.READ) WindowSize
                               -> IOQueues Queue OfficerRank (Maybe String)
                               -- ^ Write the general order index to this to open the dialog
                               -> ReactElement
officerRankAbbreviationDialog windowSizeSignal (IOQueues{input,output}) = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "OfficerRankAbbreviationDialog" constructor'

    constructor' :: ReactClassConstructor _ OfficerAbbreviationState _
    constructor' =
      let queueOpenerHandler :: _ -> OfficerRank -> Effect Unit
          queueOpenerHandler this i = setState this {rank: Just i}

          windowChangeHandler :: _ -> WindowSize -> Effect Unit
          windowChangeHandler this w = setState this {windowSize: w}

      in  ReactQ.whileMountedOne input queueOpenerHandler
            (ReactS.whileMountedIx windowSizeSignal "OfficerRankAbbreviationDialog" windowChangeHandler constructor)
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
            , state: initOfficerAbbreviationState initWindowSize
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
                          Just r -> showOfficerRankAbbreviationTitle r
                          Nothing -> ""
                      ]
                    , dialogContent_
                      [ typography {gutterBottom: true, variant: title}
                        [ text $ case mR of
                            Just r -> showChallengeOfficerRankAbbreviation r
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
