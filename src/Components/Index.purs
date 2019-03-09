module Components.Index where

import Answers.GeneralOrders (randomGeneralOrderIndex)
-- import Components.AppBar (indexAppBar)
import Components.Dialogs.GeneralOrder (generalOrderDialog)
-- import Components.Dialogs.Import (ImportDialog (..)) as Import
-- import Components.Dialogs.Export (exportDialog)
-- import Components.Snackbar (snackbars, SnackbarContent, SnackbarVariant (..))
import WithRoot (withRoot)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Exception (throwException)
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text)
import Queue.One (Queue, new, put) as Q
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

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              generateGeneralOrder :: Effect Unit
              generateGeneralOrder = runAff_ resolve do
                i <- liftEffect randomGeneralOrderIndex
                mS <- IOQueues.callAsync generalOrderQueues i
                case mS of
                  Nothing -> pure unit
                  Just s -> pure unit
          pure
            { state: {}
            , render: pure $ toElement
              -- [ indexAppBar {onImport, onExport, onNameEdit}
              [ typography {gutterBottom: true, variant: title} [text "Just a Test"]
              , button {onClick: mkEffectFn1 (const generateGeneralOrder)} [text "Random General Order"]
              , generalOrderDialog generalOrderQueues
              -- , exportDialog exportQueue
              -- , snackbars snackbarQueue
              ]
            }
