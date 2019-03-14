module Components.Pages where

import Links (Link (..))
import Links.Bootcamp (BootcampLink (..))
import Components.Snackbar (SnackbarContent)
import Components.Dialogs (DialogQueues)
import Components.Pages.Bootcamp.GeneralOrders (generalOrders)
import Components.Pages.Bootcamp.RankInsignias (rankInsignias)

import Prelude
import React (ReactElement, ReactClass, ReactClassConstructor, getState, setState, component, createLeafElement)
import React.Signal.WhileMounted (whileMountedIx)
import Queue.One (Queue) as Q
import Queue.Types (WRITE) as Q
import IxSignal (IxSignal)
import IxSignal (get) as S
import Signal.Types (READ) as S



page :: IxSignal (read :: S.READ) Link
     -> Q.Queue (write :: Q.WRITE) SnackbarContent
     -> DialogQueues
     -> ReactElement
page linkSignal snackbarQueue dialogQueues = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component "Page" constructor'
      where
        constructor' =
          let handleLink this x = setState this {currentLink: x}
          in  whileMountedIx linkSignal "Page" handleLink constructor
          where
            constructor :: ReactClassConstructor _ {currentLink :: Link} _
            constructor this = do

              initLink <- S.get linkSignal

              pure
                { state: {currentLink: initLink}
                , render: do
                  {currentLink} <- getState this
                  pure $ case currentLink of
                    Bootcamp link' -> case link' of
                      GeneralOrders -> generalOrders snackbarQueue dialogQueues.generalOrderQueues
                      RankInsignias -> rankInsignias snackbarQueue dialogQueues.rank
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }
