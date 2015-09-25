-- | This module defines foreign types and functions which wrap React's functionality.

module React
  ( ReactElement()
  , ReactThis()

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
  , GetInitialState()
  , ComponentWillMount()
  , ComponentDidMount()
  , ComponentWillReceiveProps()
  , ShouldComponentUpdate()
  , ComponentWillUpdate()
  , ComponentDidUpdate()
  , ComponentWillUnmount()

  , ReactSpec()
  , ReactClass()

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

  , handle

  , createClass
  , createElement
  , createFactory

  , render
  , renderToString
  ) where

import Prelude (Unit(), ($), bind, pure, return, unit)

import DOM (DOM())
import DOM.Node.Types (Element())

import Control.Monad.Eff (Eff())

-- | A virtual DOM node, or component.
foreign import data ReactElement :: *

-- | A reference to a component, essentially React's `this`.
foreign import data ReactThis :: * -> * -> *

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

-- | A render function.
type Render props state eff =
  ReactThis props state ->
  Eff ( props :: ReactProps props
      , refs :: ReactRefs Disallowed
      , state :: ReactState ReadOnly state
      | eff
      ) ReactElement

-- | A get initial state function.
type GetInitialState props state eff =
  ReactThis props state ->
  Eff ( props :: ReactProps props
      , state :: ReactState Disallowed state
      , refs :: ReactRefs Disallowed
      | eff
      ) state

-- | A component will mount function.
type ComponentWillMount props state eff =
  ReactThis props state ->
  Eff ( props :: ReactProps props
      , state :: ReactState ReadWrite state
      , refs :: ReactRefs Disallowed
      | eff
      ) Unit

-- | A component did mount function.
type ComponentDidMount props state eff =
  ReactThis props state ->
  Eff ( props :: ReactProps props
      , state :: ReactState ReadWrite state
      , refs :: ReactRefs ReadOnly
      | eff
      ) Unit

-- | A component will receive props function.
type ComponentWillReceiveProps props state eff =
   ReactThis props state ->
   props ->
   Eff ( props :: ReactProps props
       , state :: ReactState ReadWrite state
       , refs :: ReactRefs ReadOnly
       | eff
       ) Unit

-- | A should component update function.
type ShouldComponentUpdate props state eff =
  ReactThis props state ->
  props ->
  state ->
  Eff ( props :: ReactProps props
      , state :: ReactState ReadWrite state
      , refs :: ReactRefs ReadOnly
      | eff
      ) Boolean

-- | A component will update function.
type ComponentWillUpdate props state eff =
  ReactThis props state ->
  props ->
  state ->
  Eff ( props :: ReactProps props
      , state :: ReactState ReadWrite state
      , refs :: ReactRefs ReadOnly
      | eff
      ) Unit

-- | A component did update function.
type ComponentDidUpdate props state eff =
  ReactThis props state ->
  props ->
  state ->
  Eff ( props :: ReactProps props
      , state :: ReactState ReadOnly state
      , refs :: ReactRefs ReadOnly
      | eff
      ) Unit

-- | A component will unmount function.
type ComponentWillUnmount props state eff =
  ReactThis props state ->
  Eff ( props :: ReactProps props
      , state :: ReactState ReadOnly state
      , refs :: ReactRefs ReadOnly
      | eff
      ) Unit

-- | A specification of a component.
type ReactSpec props state eff =
  { render :: Render props state eff
  , displayName :: String
  , getInitialState :: GetInitialState props state eff
  , componentWillMount :: ComponentWillMount props state eff
  , componentDidMount :: ComponentDidMount props state eff
  , componentWillReceiveProps :: ComponentWillReceiveProps props state eff
  , shouldComponentUpdate :: ShouldComponentUpdate props state eff
  , componentWillUpdate :: ComponentWillUpdate props state eff
  , componentDidUpdate :: ComponentDidUpdate props state eff
  , componentWillUnmount :: ComponentWillUnmount props state eff
  }

-- | Create a component specification.
spec :: forall props state eff. state -> Render props state eff -> ReactSpec props state eff
spec st renderFn =
  { render:                    renderFn
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

-- | React class for components.
foreign import data ReactClass :: * -> *

-- | Read the component props.
foreign import getProps :: forall props state eff. ReactThis props state -> Eff (props :: ReactProps props | eff) props

-- | Read the component refs.
foreign import getRefs :: forall props state write eff. ReactThis props state -> Eff (refs :: ReactRefs (Read write) | eff) Refs

-- | Read the component children property.
foreign import getChildren :: forall props state eff. ReactThis props state -> Eff (props :: ReactProps props | eff) (Array ReactElement)

-- | Write the component state.
foreign import writeState :: forall props state eff. ReactThis props state -> state -> Eff (state :: ReactState ReadWrite state | eff) state

-- | Read the component state.
foreign import readState :: forall props state write eff. ReactThis props state -> Eff (state :: ReactState (Read write) state | eff) state

-- | Transform the component state by applying a function.
transformState :: forall props state eff. ReactThis props state -> (state -> state) -> Eff (state :: ReactState ReadWrite state | eff) state
transformState ctx f = do
  state <- readState ctx
  writeState ctx $ f state

-- | Create a React class from a specification.
foreign import createClass :: forall props state eff. ReactSpec props state eff -> ReactClass props

-- | Create an event handler.
foreign import handle :: forall eff ev props state result.  (ev -> EventHandlerContext eff props state result) -> EventHandler ev

-- | Create an element from a React class.
foreign import createElement :: forall props. ReactClass props -> props -> Array ReactElement -> ReactElement

-- | Create a factory from a React class.
foreign import createFactory :: forall props. ReactClass props -> props -> ReactElement

-- | Render a React element in a document element.
foreign import render :: forall eff. ReactElement -> Element -> Eff (dom :: DOM | eff) ReactElement

-- | Render a React element as a string.
foreign import renderToString :: ReactElement -> String
