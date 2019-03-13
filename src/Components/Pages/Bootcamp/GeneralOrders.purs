module Components.Pages.Bootcamp.GeneralOrders where

import Answers.Bootcamp.GeneralOrders (randomGeneralOrderIndex, challengeReport)
import Components.Snackbar (SnackbarContent)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Exception (throwException)
import React (ReactElement, ReactClass, ReactClassConstructor, createLeafElement, pureComponent, toElement)
import React.DOM (text)
import Queue.One (Queue, put) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import IOQueues (callAsync) as IOQueues
import MaterialUI.Typography (typography)
import MaterialUI.Button (button)
import MaterialUI.Enums (title)



generalOrders :: Q.Queue (write :: Q.WRITE) SnackbarContent
              -> IOQueues Q.Queue Int (Maybe String)
              -> ReactElement
generalOrders snackbarQueue generalOrderQueues = createLeafElement c {}
  where
    c :: ReactClass {}
    c = pureComponent "GeneralOrders" constructor
      where
        constructor :: ReactClassConstructor _ {} _
        constructor this = do

          let resolve eX = case eX of
                Left err -> throwException err
                Right x -> pure unit

              generateGeneralOrder :: Effect Unit
              generateGeneralOrder = runAff_ resolve do
                i <- liftEffect randomGeneralOrderIndex
                mS <- IOQueues.callAsync generalOrderQueues i
                case mS of
                  Nothing -> pure unit
                  Just s -> liftEffect $ Q.put snackbarQueue $ challengeReport i s

          pure
            { state: {}
            , render: do
              pure $ toElement
                [ typography {gutterBottom: true, variant: title} [text "Eleven General Orders"]
                , button {onClick: mkEffectFn1 (const generateGeneralOrder)} [text "Random General Order"]
                ]
            }
