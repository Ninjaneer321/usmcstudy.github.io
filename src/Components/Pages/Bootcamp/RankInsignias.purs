module Components.Pages.Bootcamp.RankInsignias (rankInsignias) where

import Answers.Bootcamp.RankInsignias
  ( EnlistedRank, EnlistedRankInsignia
  , OfficerRank, OfficerRankInsignia
  , randomEnlistedRank
  , challengeReportEnlistedRankInsignia, challengeReportEnlistedRankAbbreviation
  , checkEnlistedRankInsignia, checkEnlistedRankAbbreviation
  , indexToEnlistedRank, enlistedRankToIndex, showEnlistedRankFull
  , randomOfficerRank
  , challengeReportOfficerRankInsignia, challengeReportOfficerRankAbbreviation
  , checkOfficerRankInsignia, checkOfficerRankAbbreviation
  , indexToOfficerRank, officerRankToIndex, showOfficerRankFull
  )
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


getAllScores :: Scores -> {success :: Int, failure :: Int}
getAllScores =
  foldr (\x acc -> {success: x.success + acc.success, failure: x.failure + acc.failure})
  {success: 0, failure: 0}

rankButton :: forall r
            . (r -> Effect Unit)
           -> (r -> String)
           -> r
           -> {success :: Int, failure :: Int}
           -> ReactElement
rankButton f showR r {success,failure} = tableRow_
  [ tableCell {} $ singleton $ button {onClick: mkEffectFn1 (const (f r))} $
      singleton $ text $ showR r
  , tableCell {align: right} [text (show success)]
  , tableCell {align: right} [text (show failure)]
  ]

enlistedRankInsignias :: Scores
                      -> (EnlistedRank -> Effect Unit)
                      -> ReactElement
enlistedRankInsignias
  scores
  enlistedRankInsignia
  =
  toElement
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
        let go i = unsafePartial $ rankButton enlistedRankInsignia showEnlistedRankFull $ fromJust $ indexToEnlistedRank i
        in  mapWithIndex go scores
      ]
    , br []
    , br []
    , typography {variant: body1}
      [ text ("Successes: " <> show (getAllScores scores).success)
      ]
    , br []
    , typography {variant: body1}
      [ text ("Failures: " <> show (getAllScores scores).failure)
      ]
    ]



enlistedRankAbbreviations :: Scores
                          -> (EnlistedRank -> Effect Unit)
                          -> ReactElement
enlistedRankAbbreviations
  scores
  enlistedRankAbbreviation
  =
  toElement
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
        let go i = unsafePartial $ rankButton enlistedRankAbbreviation showEnlistedRankFull $ fromJust $ indexToEnlistedRank i
        in  mapWithIndex go scores
      ]
    , br []
    , br []
    , typography {variant: body1}
      [ text ("Successes: " <> show (getAllScores scores).success)
      ]
    , br []
    , typography {variant: body1}
      [ text ("Failures: " <> show (getAllScores scores).failure)
      ]
    ]


officerRankInsignias :: Scores
                      -> (OfficerRank -> Effect Unit)
                      -> ReactElement
officerRankInsignias
  scores
  officerRankInsignia
  =
  toElement
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
        let go i = unsafePartial $ rankButton officerRankInsignia showOfficerRankFull $ fromJust $ indexToOfficerRank i
        in  mapWithIndex go scores
      ]
    , br []
    , br []
    , typography {variant: body1}
      [ text ("Successes: " <> show (getAllScores scores).success)
      ]
    , br []
    , typography {variant: body1}
      [ text ("Failures: " <> show (getAllScores scores).failure)
      ]
    ]



officerRankAbbreviations :: Scores
                         -> (OfficerRank -> Effect Unit)
                         -> ReactElement
officerRankAbbreviations
  scores
  officerRankAbbreviation
  =
  toElement
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
        let go i = unsafePartial $ rankButton officerRankAbbreviation showOfficerRankFull $ fromJust $ indexToOfficerRank i
        in  mapWithIndex go scores
      ]
    , br []
    , br []
    , typography {variant: body1}
      [ text ("Successes: " <> show (getAllScores scores).success)
      ]
    , br []
    , typography {variant: body1}
      [ text ("Failures: " <> show (getAllScores scores).failure)
      ]
    ]



type Scores = Array {success :: Int, failure :: Int}


type State =
  { enlisted ::
    { insignias :: Scores
    , abbreviations :: Scores
    }
  , officer ::
    { insignias :: Scores
    , abbreviations :: Scores
    }
  }


initState :: State
initState =
  { enlisted:
    { insignias: enlistedScores
    , abbreviations: enlistedScores
    }
  , officer:
    { insignias: officerScores
    , abbreviations: officerScores
    }
  }
  where
    enlistedScores = replicate 12 {success: 0, failure: 0}
    officerScores = replicate 10 {success: 0, failure: 0}


rankInsignias :: Q.Queue (write :: Q.WRITE) SnackbarContent
              -> { enlisted ::
                   { insignia :: IOQueues Q.Queue EnlistedRank (Maybe EnlistedRankInsignia)
                   , abbreviation :: IOQueues Q.Queue EnlistedRank (Maybe String)
                   }
                 , officer ::
                   { insignia :: IOQueues Q.Queue OfficerRank (Maybe OfficerRankInsignia)
                   , abbreviation :: IOQueues Q.Queue OfficerRank (Maybe String)
                   }
                 }
              -> ReactElement
rankInsignias snackbarQueue dialogQueues = createLeafElement c {}
  where
    c :: ReactClass {}
    c = pureComponent "RankInsignias" constructor
      where
        constructor :: ReactClassConstructor _ State _
        constructor this = do

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              enlistedRankInsignia :: EnlistedRank -> Effect Unit
              enlistedRankInsignia r = runAff_ resolve do
                mI <- IOQueues.callAsync dialogQueues.enlisted.insignia r
                case mI of
                  Nothing -> pure unit
                  Just insig -> liftEffect do
                    {enlisted} <- getState this
                    let i = enlistedRankToIndex r
                    setState this $ case checkEnlistedRankInsignia r insig of
                      Nothing ->
                        { enlisted:
                          { insignias: unsafePartial $ fromJust $
                            modifyAt i (\x@{success} -> x {success = success + 1}) enlisted.insignias
                          , abbreviations: enlisted.abbreviations
                          }
                        }
                      Just _ ->
                        { enlisted:
                          { insignias: unsafePartial $ fromJust $
                            modifyAt i (\x@{failure} -> x {failure = failure + 1}) enlisted.insignias
                          , abbreviations: enlisted.abbreviations
                          }
                        }
                    Q.put snackbarQueue (challengeReportEnlistedRankInsignia r insig)

              generateEnlistedRankInsignia :: Effect Unit
              generateEnlistedRankInsignia = randomEnlistedRank >>= enlistedRankInsignia

              enlistedRankAbbreviation :: EnlistedRank -> Effect Unit
              enlistedRankAbbreviation r = runAff_ resolve do
                mI <- IOQueues.callAsync dialogQueues.enlisted.abbreviation r
                case mI of
                  Nothing -> pure unit
                  Just insig -> liftEffect do
                    {enlisted} <- getState this
                    let i = enlistedRankToIndex r
                    setState this $ case checkEnlistedRankAbbreviation r insig of
                      Nothing ->
                        { enlisted:
                          { abbreviations: unsafePartial $ fromJust $
                            modifyAt i (\x@{success} -> x {success = success + 1}) enlisted.abbreviations
                          , insignias: enlisted.insignias
                          }
                        }
                      Just _ ->
                        { enlisted:
                          { abbreviations: unsafePartial $ fromJust $
                            modifyAt i (\x@{failure} -> x {failure = failure + 1}) enlisted.abbreviations
                          , insignias: enlisted.insignias
                          }
                        }
                    Q.put snackbarQueue (challengeReportEnlistedRankAbbreviation r insig)

              generateEnlistedRankAbbreviation :: Effect Unit
              generateEnlistedRankAbbreviation = randomEnlistedRank >>= enlistedRankAbbreviation

              officerRankInsignia :: OfficerRank -> Effect Unit
              officerRankInsignia r = runAff_ resolve do
                mI <- IOQueues.callAsync dialogQueues.officer.insignia r
                case mI of
                  Nothing -> pure unit
                  Just insig -> liftEffect do
                    {officer} <- getState this
                    let i = officerRankToIndex r
                    setState this $ case checkOfficerRankInsignia r insig of
                      Nothing ->
                        { officer:
                          { insignias: unsafePartial $ fromJust $
                            modifyAt i (\x@{success} -> x {success = success + 1}) officer.insignias
                          , abbreviations: officer.abbreviations
                          }
                        }
                      Just _ ->
                        { officer:
                          { insignias: unsafePartial $ fromJust $
                            modifyAt i (\x@{failure} -> x {failure = failure + 1}) officer.insignias
                          , abbreviations: officer.abbreviations
                          }
                        }
                    Q.put snackbarQueue (challengeReportOfficerRankInsignia r insig)

              generateOfficerRankInsignia :: Effect Unit
              generateOfficerRankInsignia = randomOfficerRank >>= officerRankInsignia

              officerRankAbbreviation :: OfficerRank -> Effect Unit
              officerRankAbbreviation r = runAff_ resolve do
                mI <- IOQueues.callAsync dialogQueues.officer.abbreviation r
                case mI of
                  Nothing -> pure unit
                  Just insig -> liftEffect do
                    {officer} <- getState this
                    let i = officerRankToIndex r
                    setState this $ case checkOfficerRankAbbreviation r insig of
                      Nothing ->
                        { officer:
                          { abbreviations: unsafePartial $ fromJust $
                            modifyAt i (\x@{success} -> x {success = success + 1}) officer.abbreviations
                          , insignias: officer.insignias
                          }
                        }
                      Just _ ->
                        { officer:
                          { abbreviations: unsafePartial $ fromJust $
                            modifyAt i (\x@{failure} -> x {failure = failure + 1}) officer.abbreviations
                          , insignias: officer.insignias
                          }
                        }
                    Q.put snackbarQueue (challengeReportOfficerRankAbbreviation r insig)

              generateOfficerRankAbbreviation :: Effect Unit
              generateOfficerRankAbbreviation = randomOfficerRank >>= officerRankAbbreviation

          pure
            { state: initState
            , render: do
              {enlisted,officer} <- getState this
              pure $ toElement
                [ typography {gutterBottom: true, variant: title} [text "Ranks"]
                , hr []
                , typography {gutterBottom: true, variant: subheading} [text "Insignias"]
                , hr []
                , br []
                , button {onClick: mkEffectFn1 (const generateEnlistedRankInsignia)} [text "Random Enlisted Rank"]
                , br []
                , button {onClick: mkEffectFn1 (const generateOfficerRankInsignia)} [text "Random Officer Rank"]
                , enlistedRankInsignias enlisted.insignias enlistedRankInsignia
                , br []
                , officerRankInsignias officer.insignias officerRankInsignia
                , br []
                , typography {gutterBottom: true, variant: subheading} [text "Abbreviations"]
                , hr []
                , br []
                , button {onClick: mkEffectFn1 (const generateEnlistedRankAbbreviation)} [text "Random Enlisted Rank"]
                , br []
                , button {onClick: mkEffectFn1 (const generateOfficerRankAbbreviation)} [text "Random Officer Rank"]
                , enlistedRankAbbreviations enlisted.abbreviations enlistedRankAbbreviation
                , br []
                , officerRankAbbreviations officer.abbreviations officerRankAbbreviation
                ]
            }
