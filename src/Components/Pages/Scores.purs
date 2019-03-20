module Components.Pages.Scores (scores) where

import Components.Snackbar (SnackbarContent)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Array (replicate, singleton)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Exception (throwException)
import React (ReactElement, ReactClass, ReactClassConstructor, createLeafElement, pureComponent, toElement, setState, getState)
import React.DOM (text, br, span)
import React.DOM.Props (style) as RP
import Queue.One (Queue, put) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (callAsync) as IOQueues
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (right, dense, body1)
import MaterialUI.Table (table)
import MaterialUI.TableHead (tableHead_)
import MaterialUI.TableBody (tableBody_)
import MaterialUI.TableRow (tableRow_)
import MaterialUI.TableCell (tableCell)
import MaterialUI.Colors (green, red)
import Record (get) as Rec
import Data.Symbol (SProxy (..))



type Score = {success :: Int, failure :: Int}

initScore :: Score
initScore = {success: 0, failure: 0}

type Scores = Array Score

getAllScores :: Scores -> Score
getAllScores =
  foldr (\x acc -> {success: x.success + acc.success, failure: x.failure + acc.failure}) initScore



scores :: forall input output
        . { componentName    :: String
          , random :: Maybe
            { randomButtonText :: String
            , randomInput      :: Effect input
            }
          , checkChallenge   :: input -> output -> Scores -> Scores
          , challengeReport  :: input -> output -> SnackbarContent
          , buttonText       :: input -> String
          , indexToInput     :: Int -> input
          , scoresLength     :: Int
          }
       -> Q.Queue (write :: Q.WRITE) SnackbarContent
       -> IOQueues Q.Queue input (Maybe output)
       -> ReactElement
scores
  { componentName
  , random
  , checkChallenge
  , challengeReport
  , buttonText
  , indexToInput
  , scoresLength
  }
  snackbarQueue
  dialogQueues = createLeafElement c {}
  where
    c :: ReactClass {}
    c = pureComponent componentName constructor
      where
        constructor :: ReactClassConstructor _ {scores :: Scores} _
        constructor this = do

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              applyInput :: input -> Effect Unit
              applyInput i = runAff_ resolve do
                mS <- IOQueues.callAsync dialogQueues i
                case mS of
                  Nothing -> pure unit
                  Just s -> liftEffect do
                    {scores} <- getState this
                    setState this {scores: checkChallenge i s scores}
                    Q.put snackbarQueue (challengeReport i s)

          pure
            { state: {scores: replicate scoresLength initScore}
            , render: do
              let entryButton :: Int -> Score -> ReactElement
                  entryButton i {success,failure} = tableRow_
                    [ tableCell {} $ singleton $
                        let input' :: input
                            input' = indexToInput i
                        in  button
                              { onClick: mkEffectFn1 (const (applyInput input'))
                              } $ singleton $ text $ buttonText input'
                    , tableCell {align: right} [text (show success)]
                    , tableCell {align: right} [text (show failure)]
                    ]
              {scores} <- getState this
              pure $ toElement $
                [ table {padding: dense}
                  [ tableHead_ $ singleton $ tableRow_
                    [ tableCell {} $ case random of
                        Nothing -> []
                        Just {randomButtonText, randomInput} ->
                          [button {onClick: mkEffectFn1 (const (randomInput >>= applyInput))} [text randomButtonText]]
                    , tableCell {align: right} $ singleton $
                        span [RP.style {color: Rec.get (SProxy :: SProxy "700") green}] $
                          singleton $ text "✔"
                    , tableCell {align: right} $ singleton $
                        span [RP.style {color: Rec.get (SProxy :: SProxy "700") red}] $
                          singleton $ text "❌"
                    ]
                  , tableBody_ (mapWithIndex entryButton scores)
                  ]
                ] <> case scoresLength of
                  1 ->  []
                  _ ->  [ br []
                        , br []
                        , typography {variant: body1}
                          [ text ("Successes: " <> show (getAllScores scores).success)
                          ]
                        , br []
                        , typography {variant: body1}
                          [ text ("Failures: " <> show (getAllScores scores).failure)
                          ]
                        ]
            }
