module Components.Pages.Bootcamp.GeneralOrders where

import Answers.Bootcamp.GeneralOrders (randomGeneralOrderIndex, challengeReport, checkChallenge)
import Components.Snackbar (SnackbarContent)

import Prelude
import Data.Maybe (Maybe (..), fromJust)
import Data.Either (Either (..))
import Data.Array (modifyAt, replicate, singleton)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Exception (throwException, throw)
import React (ReactElement, ReactClass, ReactClassConstructor, createLeafElement, pureComponent, toElement, setState, getState)
import React.DOM (text, hr, br, span)
import React.DOM.Props (style) as RP
import Queue.One (Queue, put) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (callAsync) as IOQueues
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (title, right, dense, body1)
import MaterialUI.Typography (typography)
import MaterialUI.Table (table)
import MaterialUI.TableHead (tableHead_)
import MaterialUI.TableBody (tableBody_)
import MaterialUI.TableRow (tableRow_)
import MaterialUI.TableCell (tableCell)
import MaterialUI.Colors (green, red)
import Record (get) as Rec
import Data.Symbol (SProxy (..))
import Partial.Unsafe (unsafePartial)


type Scores = Array {success :: Int, failure :: Int}



generalOrders :: Q.Queue (write :: Q.WRITE) SnackbarContent
              -> IOQueues Q.Queue Int (Maybe String)
              -> ReactElement
generalOrders snackbarQueue generalOrderQueues = createLeafElement c {}
  where
    c :: ReactClass {}
    c = pureComponent "GeneralOrders" constructor
      where
        constructor :: ReactClassConstructor _ {scores :: Scores} _
        constructor this = do

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              generalOrder :: Int -> Effect Unit
              generalOrder i = runAff_ resolve do
                mS <- IOQueues.callAsync generalOrderQueues i
                case mS of
                  Nothing -> pure unit
                  Just s -> liftEffect do
                    {scores} <- getState this
                    case checkChallenge i s of
                      Nothing -> throw $ "No general order with index " <> show i
                      Just mValid ->
                        setState this $ case mValid of
                          Nothing ->
                            { scores: unsafePartial $ fromJust $
                              modifyAt (i - 1) (\x@{success} -> x {success = success + 1}) scores
                            }
                          Just _ ->
                            { scores: unsafePartial $ fromJust $
                              modifyAt (i - 1) (\x@{failure} -> x {failure = failure + 1}) scores
                            }
                    Q.put snackbarQueue $ challengeReport i s

              generateGeneralOrder :: Effect Unit
              generateGeneralOrder = randomGeneralOrderIndex >>= generalOrder

          pure
            { state: {scores: replicate 11 {success: 0, failure: 0}}
            , render: do
              {scores} <- getState this
              let generalOrderButton i {success,failure} = tableRow_
                    [ tableCell {} $ singleton $ button {onClick: mkEffectFn1 (const (generalOrder i))} $
                        singleton $ text $ case i of
                          1 -> "First"
                          2 -> "Second"
                          3 -> "Third"
                          4 -> "Fourth"
                          5 -> "Fifth"
                          6 -> "Sixth"
                          7 -> "Seventh"
                          8 -> "Eighth"
                          9 -> "Ninth"
                          10 -> "Tenth"
                          11 -> "Eleventh"
                          _ -> ""
                    , tableCell {align: right} [text (show success)]
                    , tableCell {align: right} [text (show failure)]
                    ]
              let allScores = foldr (\x acc -> {success: x.success + acc.success, failure: x.failure + acc.failure})
                    {success: 0, failure: 0} scores
              pure $ toElement
                [ typography {gutterBottom: true, variant: title} [text "Eleven General Orders of a Sentry"]
                , hr []
                , table {padding: dense}
                  [ tableHead_ $ singleton $ tableRow_
                    [ tableCell {} [text ""]
                    , tableCell {align: right} $ singleton $
                        span [RP.style {color: Rec.get (SProxy :: SProxy "700") green}] $
                          singleton $ text "✔"
                    , tableCell {align: right} $ singleton $
                        span [RP.style {color: Rec.get (SProxy :: SProxy "700") red}] $
                          singleton $ text "❌"
                    ]
                  , tableBody_ (mapWithIndex (\i -> generalOrderButton (i + 1)) scores)
                  ]
                , br []
                , button {onClick: mkEffectFn1 (const generateGeneralOrder)} [text "Random General Order"]
                , br []
                , br []
                , typography {variant: body1}
                  [ text ("Successes: " <> show allScores.success)
                  ]
                , br []
                , typography {variant: body1}
                  [ text ("Failures: " <> show allScores.failure)
                  ]
                ]
            }
