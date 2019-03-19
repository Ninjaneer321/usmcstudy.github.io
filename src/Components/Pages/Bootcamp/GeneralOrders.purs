module Components.Pages.Bootcamp.GeneralOrders where

import Answers.Bootcamp.GeneralOrders (randomGeneralOrderIndex, challengeReport, checkChallenge)
import Components.Pages.Scores (scores)
import Components.Snackbar (SnackbarContent)

import Prelude
import Data.Maybe (Maybe (..), fromJust)
import Data.Array (modifyAt)
import Data.Int.Prose (proseInt)
import React (ReactElement, toElement)
import React.DOM (text, hr, br)
import Queue.One (Queue) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)
import Partial.Unsafe (unsafePartial)




generalOrders :: Q.Queue (write :: Q.WRITE) SnackbarContent
              -> IOQueues Q.Queue Int (Maybe String)
              -> ReactElement
generalOrders snackbarQueue generalOrderQueues =
  let scores' = scores
        { componentName: "GeneralOrders"
        , randomButtonText: "Random General Order"
        , randomInput: randomGeneralOrderIndex
        , checkChallenge: \i s scores ->
            let go x@{success,failure} = case checkChallenge i s of
                  Nothing -> x {success = success + 1}
                  Just _ -> x {failure = failure + 1}
            in  unsafePartial $ fromJust $ modifyAt (i - 1) go scores
        , challengeReport
        , buttonText: proseInt true
        , indexToInput: \i -> i + 1
        , scoresLength: 11
        } snackbarQueue generalOrderQueues
  in  toElement
        [ typography {gutterBottom: true, variant: title} [text "Eleven General Orders of a Sentry"]
        , hr []
        , br []
        , scores'
        ]
