module Components.Pages.Bootcamp.RankInsignias where

import Answers.Bootcamp.RankInsignias
  ( EnlistedRank, EnlistedRankInsignia, randomEnlistedRank
  , challengeReportEnlistedRankInsignia, challengeReportEnlistedRankAbbreviation
  , checkEnlistedRankInsignia, checkEnlistedRankAbbreviation
  , indexToEnlistedRank, enlistedRankToIndex, showEnlistedRankFull)
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
import Effect.Exception (throwException)
import React (ReactElement, ReactClass, ReactClassConstructor, createLeafElement, pureComponent, toElement, setState, getState)
import React.DOM (text, hr, br, span)
import React.DOM.Props (style) as RP
import Queue.One (Queue, put) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (callAsync) as IOQueues
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (title, subheading, right, dense, body1)
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


type EnlistedState =
  { enlistedInsigniaScores :: Scores
  , enlistedAbbreviationScores :: Scores
  }


initEnlistedState :: EnlistedState
initEnlistedState =
  { enlistedInsigniaScores: replicate 12 {success: 0, failure: 0}
  , enlistedAbbreviationScores: replicate 12 {success: 0, failure: 0}
  }


enlistedRankInsignias :: Q.Queue (write :: Q.WRITE) SnackbarContent
                      -> IOQueues Q.Queue EnlistedRank (Maybe EnlistedRankInsignia)
                      -- -> IOQueues Q.Queue EnlistedRank (Maybe String)
                      -> ReactElement
enlistedRankInsignias
  snackbarQueue
  enlistedRankInsigniaQueues
  -- enlistedRankAbbreviationQueues
  = createLeafElement c {}
  where
    c :: ReactClass {}
    c = pureComponent "RankInsignias" constructor
      where
        constructor :: ReactClassConstructor _ EnlistedState _
        constructor this = do

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              enlistedRankInsignia :: EnlistedRank -> Effect Unit
              enlistedRankInsignia r = runAff_ resolve do
                mI <- IOQueues.callAsync enlistedRankInsigniaQueues r
                case mI of
                  Nothing -> pure unit
                  Just insig -> liftEffect do
                    {enlistedInsigniaScores} <- getState this
                    let i = enlistedRankToIndex r
                    setState this $ case checkEnlistedRankInsignia r insig of
                      Nothing ->
                        { enlistedInsigniaScores: unsafePartial $ fromJust $
                          modifyAt i (\x@{success} -> x {success = success + 1}) enlistedInsigniaScores
                        }
                      Just _ ->
                        { enlistedInsigniaScores: unsafePartial $ fromJust $
                          modifyAt i (\x@{failure} -> x {failure = failure + 1}) enlistedInsigniaScores
                        }
                    Q.put snackbarQueue (challengeReportEnlistedRankInsignia r insig)

              -- enlistedRankAbbreviation :: EnlistedRank -> Effect Unit
              -- enlistedRankAbbreviation r = runAff_ resolve do
              --   mI <- IOQueues.callAsync enlistedRankAbbreviationQueues r
              --   case mI of
              --     Nothing -> pure unit
              --     Just insig -> liftEffect do
              --       {enlistedAbbreviationScores} <- getState this
              --       let i = enlistedRankToIndex r
              --       setState this $ case checkEnlistedRankAbbreviation r insig of
              --         Nothing ->
              --           { enlistedAbbreviationScores: unsafePartial $ fromJust $
              --             modifyAt i (\x@{success} -> x {success = success + 1}) enlistedAbbreviationScores
              --           }
              --         Just _ ->
              --           { enlistedAbbreviationScores: unsafePartial $ fromJust $
              --             modifyAt i (\x@{failure} -> x {failure = failure + 1}) enlistedAbbreviationScores
              --           }
              --       Q.put snackbarQueue (challengeReportEnlistedRankAbbreviation r insig)

              generateEnlistedRankInsignia :: Effect Unit
              generateEnlistedRankInsignia = randomEnlistedRank >>= enlistedRankInsignia

              -- generateEnlistedRankAbbreviation :: Effect Unit
              -- generateEnlistedRankAbbreviation = randomEnlistedRank >>= enlistedRankAbbreviation

          pure
            { state: initEnlistedState
            , render: do
              scores <- getState this
              let enlistedRankButton f r {success,failure} = tableRow_
                    [ tableCell {} $ singleton $ button {onClick: mkEffectFn1 (const (f r))} $
                        singleton $ text $ showEnlistedRankFull r
                    , tableCell {align: right} [text (show success)]
                    , tableCell {align: right} [text (show failure)]
                    ]
                  getAllScores =
                    foldr (\x acc -> {success: x.success + acc.success, failure: x.failure + acc.failure})
                    {success: 0, failure: 0}
              pure $ toElement
                [ table {padding: dense}
                  [ tableHead_ $ singleton $ tableRow_
                    [ tableCell {} [text ""]
                    , tableCell {align: right} $ singleton $
                        span [RP.style {color: Rec.get (SProxy :: SProxy "700") green}] $
                          singleton $ text "✔"
                    , tableCell {align: right} $ singleton $
                        span [RP.style {color: Rec.get (SProxy :: SProxy "700") red}] $
                          singleton $ text "❌"
                    ]
                  , tableBody_ $
                    let go i = unsafePartial $ enlistedRankButton enlistedRankInsignia $ fromJust $ indexToEnlistedRank i
                    in  mapWithIndex go scores.enlistedInsigniaScores
                  ]
                , br []
                , button {onClick: mkEffectFn1 (const generateEnlistedRankInsignia)} [text "Random Enlisted Rank"]
                , br []
                , br []
                , typography {variant: body1}
                  [ text ("Successes: " <> show (getAllScores scores.enlistedInsigniaScores).success)
                  ]
                , br []
                , typography {variant: body1}
                  [ text ("Failures: " <> show (getAllScores scores.enlistedInsigniaScores).failure)
                  ]
                ]
            }




rankInsignias :: Q.Queue (write :: Q.WRITE) SnackbarContent
              -> { enlisted ::
                   { insignia :: IOQueues Q.Queue EnlistedRank (Maybe EnlistedRankInsignia)
                   -- , abbreviation :: IOQueues Q.Queue EnlistedRank (Maybe String)
                   }
                 }
              -> ReactElement
rankInsignias snackbarQueue dialogQueues = toElement
  [ typography {gutterBottom: true, variant: title} [text "Rank Insignias"]
  , hr []
  , typography {gutterBottom: true, variant: subheading} [text "Enlisted Ranks"]
  , hr []
  , enlistedRankInsignias snackbarQueue dialogQueues.enlisted.insignia
  ]
