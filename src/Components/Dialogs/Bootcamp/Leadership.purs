module Components.Dialogs.Bootcamp.Leadership where

import Answers.Bootcamp.Leadership
  ( showLeadershipTraitsTitle, showChallengeTraits
  , showLeadershipPrincipalTitle, showChallengePrincipal
  )
import Window.Size (WindowSize)
import Components.Dialogs.Generic (genericDialog, intToStringDialog)

import Prelude
import Data.Maybe (Maybe (..))
import Data.String (split)
import Data.String.Common (trim)
import Data.String.Pattern (Pattern (..))
import Data.String.Yarn (words, lines)
import Data.Set (Set, fromFoldable)
import Data.Array (length)
import Effect.Uncurried (mkEffectFn1)
import Queue.One (Queue)
import IOQueues (IOQueues)
import IxSignal (IxSignal)
import Signal.Types (READ) as S
import React (ReactElement, setState, getState)
import React.DOM (text)
import React.SyntheticEvent (target)
import MaterialUI.Typography (typography)
import MaterialUI.TextField (textField')
import MaterialUI.Enums (title, body1)
import Unsafe.Coerce (unsafeCoerce)




leadershipTraitsDialog :: IxSignal (read :: S.READ) WindowSize
                       -> IOQueues Queue Unit (Maybe (Set String))
                       -> ReactElement
leadershipTraitsDialog = genericDialog
  { componentName: "LeadershipTraitsDialog"
  , titleName: "leadership-traits-dialog-title"
  , initialState: {value: ""}
  , title: \this -> do
      {open} <- getState this
      pure
        [ text $ case open of
            Just _ -> showLeadershipTraitsTitle
            Nothing -> ""
        ]
  , content: \this -> do
      let changedValue e = do
            t <- target e
            setState this {state: {value: (unsafeCoerce t).value}}
      {open} <- getState this
      pure
        [ typography {gutterBottom: true, variant: title}
          [ text $ case open of
              Just _ -> showChallengeTraits
              Nothing -> ""
          ]
        , textField' {onChange: mkEffectFn1 changedValue, fullWidth: true, multiline: true}
        , typography {variant: body1}
          [text "Use spaces, commas, or newlines to separate the traits."]
        ]
  , getOutput: \{value} -> fromFoldable $
    let overNewline = lines value
    in  if length overNewline == 1
          then let overCommas = split (Pattern ",") value
               in  if length overCommas == 1
                     then words value
                     else map trim overCommas
          else map trim overNewline
  }



leadershipPrincipalDialog :: IxSignal (read :: S.READ) WindowSize
                          -> IOQueues Queue Int (Maybe String)
                          -> ReactElement
leadershipPrincipalDialog = intToStringDialog
  { componentName: "LeadershipPrincipalDialog"
  , titleName: "leadership-principal-dialog-title"
  , title: showLeadershipPrincipalTitle
  , content: showChallengePrincipal
  }
