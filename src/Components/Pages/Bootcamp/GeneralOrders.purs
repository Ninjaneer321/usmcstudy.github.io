module Components.Pages.Bootcamp.GeneralOrders where

import Answers.Class (randomChallenge, report, checkChallenge)
import Answers.Bootcamp.GeneralOrders (GeneralOrder (..))
import Components.Pages.Scores (scores)
import Components.Snackbar (SnackbarContent)

import Prelude
import Data.Maybe (Maybe (..), fromJust)
import Data.Array (modifyAt)
import Data.Int.Prose (proseInt)
import Data.Indexable (toIndex, fromIndex)
import React (ReactElement, toElement)
import React.DOM (text, hr, br)
import Queue.One (Queue) as Q
import Queue.Types (WRITE) as Q
import IOQueues (IOQueues)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)
import Partial.Unsafe (unsafePartial)




generalOrders :: Q.Queue (write :: Q.WRITE) SnackbarContent
              -> IOQueues Q.Queue GeneralOrder (Maybe String)
              -> ReactElement
generalOrders snackbarQueue generalOrderQueues =
  let scores' = scores
        { componentName: "GeneralOrders"
        , random: Just
          { randomButtonText: "Random"
          , randomInput: randomChallenge
          }
        , checkChallenge: \i s scores ->
            let go x@{success,failure} = case checkChallenge i s of
                  Nothing -> x {success = success + 1}
                  Just _ -> x {failure = failure + 1}
            in  unsafePartial $ fromJust $ modifyAt (toIndex i) go scores -- FIXME use ToIndex class
        , challengeReport: report -- FIXME use class
        , buttonText: \(GeneralOrder i) -> proseInt true i
        , indexToInput: \i -> unsafePartial $ fromJust $ fromIndex i -- FIXME use FromIndex class
        , scoresLength: 11 -- FIXME make class for this
        } snackbarQueue generalOrderQueues
  in  toElement
        [ typography {gutterBottom: true, variant: title} [text "Eleven General Orders of a Sentry"]
        , hr []
        , br []
        , scores'
        ]
