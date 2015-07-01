-- | This module defines foreign types and functions which wrap React's functionality.

module React 
  ( UI()
  , UIRef()
  
  , EventHandler()
  
  , Disallowed()
  , ReadAllowed()
  , WriteAllowed()
  
  , ReactState()
  , ReactProps()
  , ReactRefs()
  
  , Render()
  
  , UISpec()
  
  , Event()
  , MouseEvent()
  , KeyboardEvent()
  
  , EventHandlerContext()
  
  , spec
  
  , getProps
  , getRefs
  
  , readState
  , writeState
  , transformState
  
  , mkUI
  
  , handle
  
  , renderToString
  , renderToBody
  , renderToElementById
  ) where

import Prelude

import DOM

import Control.Monad.Eff
import Control.Monad.Eff.Console

-- | A virtual DOM node, or component.
foreign import data UI :: *

-- | A reference to a component, essentially React's `this`.
foreign import data UIRef :: *

-- | An event handler. The type argument represents the type of the event.
foreign import data EventHandler :: * -> *

-- | This phantom effect indicates that both read and write access to a resource are disallowed.
foreign import data Disallowed :: !

-- | This phantom effect indicates that only read access to a resource is allowed.
foreign import data ReadAllowed :: !

-- | This phantom effect indicates that only write access to a resource is allowed.
foreign import data WriteAllowed :: !

-- | This effect indicates that a computation may read or write the component state.
foreign import data ReactState :: # ! -> * -> !

-- | This effect indicates that a computation may read the component props.
foreign import data ReactProps :: * -> !

-- | This effect indicates that a computation may read the component refs.
foreign import data ReactRefs :: * -> !

-- | The type of DOM events.
foreign import data Event :: *

-- | The type of mouse events.
type MouseEvent = 
  { pageX :: Number
  , pageY :: Number 
  }

-- | The type of keyboard events.
type KeyboardEvent = 
  { altKey   :: Boolean
  , ctrlKey  :: Boolean
  , charCode :: Int
  , key      :: String
  , keyCode  :: Int
  , locale   :: String
  , location :: Int
  , metaKey  :: Boolean
  , repeat   :: Boolean
  , shiftKey :: Boolean
  , which    :: Int
  }

-- | A function which handles events.
type EventHandlerContext eff props refs state result =  
  Eff ( props :: ReactProps props
      , refs :: ReactRefs refs
      , state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state
      | eff
      ) result

-- | A rendering function.
type Render props refs state eff =
  UIRef ->
  Eff ( props :: ReactProps props
      , refs :: Disallowed
      , state :: ReactState (read :: ReadAllowed) state
      | eff
      ) UI

-- | A specification of a component.
type UISpec props refs state eff =
  { getInitialState
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: Disallowed
             , refs :: Disallowed
             | eff
             ) state
  , componentWillMount
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state
             , refs :: Disallowed
             | eff
             ) Unit
  , componentDidMount
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state
             , refs :: ReactRefs refs
             | eff
             ) Unit
  , componentWillReceiveProps
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state
             , refs :: ReactRefs refs
             | eff
             ) Unit
  , shouldComponentUpdate
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state
             , refs :: ReactRefs refs
             | eff
             ) Boolean
  , componentWillUpdate
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state
             , refs :: ReactRefs refs
             | eff
             ) Unit
  , componentDidUpdate
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState (read :: ReadAllowed) state
             , refs :: ReactRefs refs
             | eff
             ) Unit
  , componentWillUnmount
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState (read :: ReadAllowed) state
             , refs :: ReactRefs refs
             | eff
             ) Unit
  }

-- | Create a component specification.
spec :: forall props refs state eff. state -> UISpec props refs state eff
spec st =
  { getInitialState:           \_ -> pure st
  , componentWillMount:        \_ -> return unit
  , componentDidMount:         \_ -> return unit
  , componentWillReceiveProps: \_ -> return unit
  , shouldComponentUpdate:     \_ -> return true
  , componentWillUpdate:       \_ -> return unit
  , componentDidUpdate:        \_ -> return unit
  , componentWillUnmount:      \_ -> return unit
  }

-- | Read the component props.
foreign import getProps :: forall props eff. 
                             UIRef -> 
                             Eff (props :: ReactProps props | eff) props

-- | Read the component refs.
foreign import getRefs :: forall refs eff. UIRef -> Eff (refs :: ReactRefs refs | eff) refs

-- | Write the component state.
foreign import writeState :: forall state eff. 
                               UIRef -> 
                               state -> 
                               Eff ( state :: ReactState ( read :: ReadAllowed
                                                         , write :: WriteAllowed
                                                         ) state 
                                   | eff
                                   ) state

-- | Read the component state.
foreign import readState :: forall state stateEff eff. 
                              UIRef ->
                              Eff ( state :: ReactState (read :: ReadAllowed | stateEff) state 
                                  | eff
                                  ) state

-- | Transform the component state by applying a function.
transformState :: forall state statePerms eff. 
                    UIRef ->
                    (state -> state) -> 
                    Eff ( state :: ReactState ( read :: ReadAllowed
                                              , write :: WriteAllowed
                                              ) state
                        | eff
                        ) state
transformState ctx f = do
  state <- readState ctx
  writeState ctx $ f state

-- | Create a component from a component spec.
foreign import mkUI :: forall props refs state eff.
                         UISpec props refs state eff ->
                         Render props refs state eff ->
                         props -> 
                         UI

-- | Create an event handler.
foreign import handle :: forall eff ev props refs state result.
                           (ev -> EventHandlerContext eff props refs state result) ->
                           EventHandler ev

-- | Render a component as a string.
foreign import renderToString :: UI -> String

-- | Render a component to the document body.
foreign import renderToBody :: forall eff. UI -> Eff (dom :: DOM | eff) UI

-- | Render a component to the element with the specified ID.
foreign import renderToElementById :: forall eff. String -> UI -> Eff (dom :: DOM | eff) UI
