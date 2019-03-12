module Components.Index where

import Answers.GeneralOrders (randomGeneralOrderIndex, checkChallenge, challengeReport)
import Components.AppBar (indexAppBar)
import Components.Dialogs.GeneralOrder (generalOrderDialog)
-- import Components.Dialogs.Import (ImportDialog (..)) as Import
-- import Components.Dialogs.Export (exportDialog)
import Components.Snackbar (snackbars, SnackbarContent, SnackbarVariant (Success, Error))
import Window.Size (windowSizeSignal) as Window
import Links (linkSignal) as Links
import WithRoot (withRoot)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Time.Duration (Milliseconds (..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Exception (throwException, throw)
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text)
import Queue.One (Queue, new, put) as Q
import Queue.Types (writeOnly, WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (new, callAsync) as IOQueues
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (title)



index :: ReactElement
index = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> do
          ( generalOrderQueues :: IOQueues Q.Queue Int (Maybe String)
            ) <- IOQueues.new
          ( snackbarQueue :: Q.Queue (write :: Q.WRITE) SnackbarContent
            ) <- Q.writeOnly <$> Q.new
          windowSizeSignal <- Window.windowSizeSignal
          linkSignal <- Links.linkSignal

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              generateGeneralOrder :: Effect Unit
              generateGeneralOrder = runAff_ resolve do
                i <- liftEffect randomGeneralOrderIndex
                mS <- IOQueues.callAsync generalOrderQueues i
                case mS of
                  Nothing -> pure unit
                  Just s -> liftEffect $ Q.put snackbarQueue $ challengeReport i s -- case checkChallenge i s of
                    -- Nothing -> liftEffect $ throw $ "No general order with index " <> show i
                    -- Just valid -> liftEffect $ Q.put snackbarQueue
                    --   { variant: if valid then Success else Error
                    --   , timeout: if valid
                    --     then Just (Milliseconds 3000.0)
                    --     else Nothing
                    --   , message:
                    --     if valid
                    --       then "Correct!"
                    --       else "Incorrect."
                    --   }
          pure
            { state: {}
            , render: pure $ toElement
              [ indexAppBar windowSizeSignal linkSignal
              , typography {gutterBottom: true, variant: title} [text "Eleven General Orders"]
              , button {onClick: mkEffectFn1 (const generateGeneralOrder)} [text "Random General Order"]
              , generalOrderDialog generalOrderQueues windowSizeSignal
              -- , exportDialog exportQueue
              , snackbars snackbarQueue
              ]
            }
