-- | This module defines foreign types and functions which wrap React's functionality.

module React
  ( UI()
  , UIRef()

  , EventHandler()

  , Disallowed()
  , Read()
  , Write()
  , Only()
  , ReadWrite()
  , ReadOnly()

  , ReactState()
  , ReactProps()
  , ReactRefs()

  , Refs()

  , Render()

  , UISpec()
  , UIFactory()

  , Event()
  , MouseEvent()
  , KeyboardEvent()

  , EventHandlerContext()

  , spec

  , getProps
  , getRefs
  , getChildren

  , readState
  , writeState
  , transformState

  , mkUI

  , handle

  , renderToString
  , renderToBody
  , renderToElementById
  , createElement
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

-- | This phantom type indicates that both read and write access to a resource are disallowed.
data Disallowed

-- | This phantom type indicates that read access to a resource is allowed.
data Read write

-- | This phantom type indicates that write access to a resource is allowed.
data Write

-- | This phantom type indicates that only read access to a resource is allowed.
data Only

-- | An access synonym which indicates that both read and write access are allowed.
type ReadWrite = Read Write

-- | An access synonym which indicates that reads are allowed but writes are not.
type ReadOnly = Read Only

-- | This effect indicates that a computation may read or write the component state.
-- |
-- | The first type argument is either `ReadWrite`, `ReadOnly` or `Disallowed` dependeding on the context.
-- |
-- | The second type argument is the type of the state of the component.
foreign import data ReactState :: * -> * -> !

-- | This effect indicates that a computation may read the component props.
foreign import data ReactProps :: * -> !

-- | This effect indicates that a computation may read the component refs.
-- |
-- | The first type argument is either `ReadOnly` or `Disallowed` dependeding on the context.
foreign import data ReactRefs :: * -> !

-- | The type of refs objects.
foreign import data Refs :: *

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
type EventHandlerContext eff props state result =
  Eff ( props :: ReactProps props
      , refs :: ReactRefs ReadOnly
      , state :: ReactState ReadWrite state
      | eff
      ) result

-- | A rendering function.
type Render props state eff =
  UIRef ->
  Eff ( props :: ReactProps props
      , refs :: ReactRefs Disallowed
      , state :: ReactState ReadOnly state
      | eff
      ) UI

-- | A specification of a component.
type UISpec props state eff =
  { render :: Render props state eff
  , displayName :: String
  , getInitialState
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState Disallowed state
             , refs :: ReactRefs Disallowed
             | eff
             ) state
  , componentWillMount
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState ReadWrite state
             , refs :: ReactRefs Disallowed
             | eff
             ) Unit
  , componentDidMount
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState ReadWrite state
             , refs :: ReactRefs ReadOnly
             | eff
             ) Unit
  , componentWillReceiveProps
      :: UIRef ->
         props ->
         Eff ( props :: ReactProps props
             , state :: ReactState ReadWrite state
             , refs :: ReactRefs ReadOnly
             | eff
             ) Unit
  , shouldComponentUpdate
      :: UIRef ->
         props ->
         state ->
         Eff ( props :: ReactProps props
             , state :: ReactState ReadWrite state
             , refs :: ReactRefs ReadOnly
             | eff
             ) Boolean
  , componentWillUpdate
      :: UIRef ->
         props ->
         state ->
         Eff ( props :: ReactProps props
             , state :: ReactState ReadWrite state
             , refs :: ReactRefs ReadOnly
             | eff
             ) Unit
  , componentDidUpdate
      :: UIRef ->
         props ->
         state ->
         Eff ( props :: ReactProps props
             , state :: ReactState ReadOnly state
             , refs :: ReactRefs ReadOnly
             | eff
             ) Unit
  , componentWillUnmount
      :: UIRef ->
         Eff ( props :: ReactProps props
             , state :: ReactState ReadOnly state
             , refs :: ReactRefs ReadOnly
             | eff
             ) Unit
  }

-- | Factory function for components.
type UIFactory props = props -> UI

-- | Create a component specification.
spec :: forall props state eff. state -> Render props state eff -> UISpec props state eff
spec st render =
  { render:                    render
  , displayName:               ""
  , getInitialState:           \_ -> pure st
  , componentWillMount:        \_ -> return unit
  , componentDidMount:         \_ -> return unit
  , componentWillReceiveProps: \_ _ -> return unit
  , shouldComponentUpdate:     \_ _ _ -> return true
  , componentWillUpdate:       \_ _ _ -> return unit
  , componentDidUpdate:        \_ _ _ -> return unit
  , componentWillUnmount:      \_ -> return unit
  }

-- | Read the component props.
foreign import getProps :: forall props eff.
                             UIRef ->
                             Eff (props :: ReactProps props | eff) props

-- | Read the component refs.
foreign import getRefs :: forall write eff.
                            UIRef ->
                            Eff (refs :: ReactRefs (Read write) | eff) Refs

-- | Read the component children property.
foreign import getChildren :: forall props eff.
                                UIRef ->
                                Eff (props :: ReactProps props | eff) (Array UI)

-- | Write the component state.
foreign import writeState :: forall state eff.
                               UIRef ->
                               state ->
                               Eff (state :: ReactState ReadWrite state | eff) state

-- | Read the component state.
foreign import readState :: forall state write eff.
                              UIRef ->
                              Eff (state :: ReactState (Read write) state | eff) state

-- | Transform the component state by applying a function.
transformState :: forall state statePerms eff.
                    UIRef ->
                    (state -> state) ->
                    Eff (state :: ReactState ReadWrite state | eff) state
transformState ctx f = do
  state <- readState ctx
  writeState ctx $ f state

-- | Create a component from a component spec.
foreign import mkUI :: forall props state eff.
                         UISpec props state eff ->
                         UIFactory props

-- | Create an event handler.
foreign import handle :: forall eff ev props state result.
                           (ev -> EventHandlerContext eff props state result) ->
                           EventHandler ev

-- | Render a component as a string.
foreign import renderToString :: UI -> String

-- | Render a component to the document body.
foreign import renderToBody :: forall eff. UI -> Eff (dom :: DOM | eff) UI

-- | Render a component to the element with the specified ID.
foreign import renderToElementById :: forall eff. String -> UI -> Eff (dom :: DOM | eff) UI

-- | Create an element from a component factory.
foreign import createElement :: forall props. UIFactory props -> props -> Array UI -> UI
