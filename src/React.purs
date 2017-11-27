-- | This module defines foreign types and functions which wrap React's functionality.

module React
  ( class ReactRender
  , ReactElement
  , ReactComponent
  , ReactThis
  , TagName

  , EventHandler

  , Read
  , Write
  , Disallowed
  , ReadWrite
  , ReadOnly

  , ReactState
  , ReactProps

  , Render
  , ComponentWillMount
  , ComponentDidMount
  , ComponentDidCatch
  , ComponentWillReceiveProps
  , ShouldComponentUpdate
  , ComponentWillUpdate
  , ComponentDidUpdate
  , ComponentWillUnmount

  , ReactClass

  , Event
  , MouseEvent
  , KeyboardEvent

  , EventHandlerContext

  , component
  , pureComponent

  , getProps
  , getChildren

  , readState
  , writeState
  , writeStateWithCallback
  , transformState

  , forceUpdate
  , forceUpdateCb

  , handle
  , preventDefault
  , stopPropagation

  , createElement
  , createElementDynamic
  , createElementTagName
  , createElementTagNameDynamic

  , Children
  , childrenToArray
  ) where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Type.Row (class RowLacks)

-- | Name of a tag.
type TagName = String

-- | A virtual DOM node, or component.
foreign import data ReactElement :: Type

-- | A mounted react component
foreign import data ReactComponent :: Type

-- | A reference to a component, essentially React's `this`.
foreign import data ReactThis :: Type -> Type -> Type

-- | An event handler. The type argument represents the type of the event.
foreign import data EventHandler :: Type -> Type

-- | This phantom type indicates that read access to a resource is allowed.
foreign import data Read :: Effect

-- | This phantom type indicates that write access to a resource is allowed.
foreign import data Write :: Effect

-- | An access synonym which indicates that neither read nor write access are allowed.
type Disallowed = () :: # Effect

-- | An access synonym which indicates that both read and write access are allowed.
type ReadWrite = (read :: Read, write :: Write)

-- | An access synonym which indicates that reads are allowed but writes are not.
type ReadOnly = (read :: Read)

-- | This effect indicates that a computation may read or write the component state.
-- |
-- | The first type argument is a row of access types (`Read`, `Write`).
foreign import data ReactState :: # Effect -> Effect

-- | This effect indicates that a computation may read the component props.
foreign import data ReactProps :: Effect

-- | The type of DOM events.
foreign import data Event :: Type

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
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) result

class ReactRender a

instance arrayReactRender :: ReactRender (Array ReactElement)

instance reactElementReactRender :: ReactRender ReactElement

instance stringReactRender :: ReactRender String

instance intReactRender :: ReactRender Int

instance numberReactRender :: ReactRender Number

-- | A render function.
type Render render eff =
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    | eff
    ) render

-- | A component will mount function.
type ComponentWillMount eff =
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) Unit

-- | A component did mount function.
type ComponentDidMount eff =
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) Unit

type ComponentDidCatch eff =
  Error ->
  { componentStack :: String } ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) Unit

-- | A component will receive props function.
type ComponentWillReceiveProps props eff =
   props ->
   Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) Unit

-- | A should component update function.
type ShouldComponentUpdate props state eff =
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) Boolean

-- | A component will update function.
type ComponentWillUpdate props state eff =
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) Unit

-- | A component did update function.
type ComponentDidUpdate props state eff =
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    | eff
    ) Unit

-- | A component will unmount function.
type ComponentWillUnmount eff =
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    | eff
    ) Unit

-- | Required fields for constructing a ReactClass.
type ReactSpecRequired state render eff r =
  ( state :: state
  , render :: Render render eff
  | r
  )

-- | Optional fields for constructing a ReactClass.
type ReactSpecOptional props state eff r =
  ( componentWillMount :: ComponentWillMount eff
  , componentDidMount :: ComponentDidMount eff
  , componentDidCatch :: ComponentDidCatch eff
  , componentWillReceiveProps :: ComponentWillReceiveProps props eff
  , componentWillUpdate :: ComponentWillUpdate props state eff
  , componentDidUpdate :: ComponentDidUpdate props state eff
  , componentWillUnmount :: ComponentWillUnmount eff
  | r
  )

type ReactSpecShouldComponentUpdate props state eff =
  ( shouldComponentUpdate :: ShouldComponentUpdate props state eff
  )

type ReactSpecAll props state render eff =
  ReactSpecRequired state render eff (ReactSpecOptional props state eff (ReactSpecShouldComponentUpdate props state eff))

type ReactSpecPure props state render eff =
  ReactSpecRequired state render eff (ReactSpecOptional props state eff ())

-- | The signature for a ReactClass constructor. A constructor takes the
-- | `ReactThis` context and returns a record with appropriate lifecycle
-- | methods.
type ReactClassConstructor props state render eff r =
  ReactThis props state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState Disallowed
    | eff
    ) (Record (ReactSpecRequired state render eff r))

-- | Creates a `ReactClass`` inherited from `React.Component`.
component
  :: forall props state render eff r x
   . Union (ReactSpecRequired (Record state) render eff r) x (ReactSpecAll (Record props) (Record state) render eff)
  => RowLacks "children" props
  => RowLacks "key" props
  => ReactRender render
  => String
  -> ReactClassConstructor (Record props) (Record state) render eff r
  -> ReactClass (Record props)
component = componentImpl

-- | Creates a `ReactClass`` inherited from `React.PureComponent`.
pureComponent
  :: forall props state render eff r x
   . Union (ReactSpecRequired (Record state) render eff r) x (ReactSpecPure (Record props) (Record state) render eff)
  => RowLacks "children" props
  => RowLacks "key" props
  => ReactRender render
  => String
  -> ReactClassConstructor (Record props) (Record state) render eff r
  -> ReactClass (Record props)
pureComponent = pureComponentImpl

foreign import componentImpl :: forall this props eff r.
  String ->
  (this -> Eff eff r) ->
  ReactClass props

foreign import pureComponentImpl :: forall this props eff r.
  String ->
  (this -> Eff eff r) ->
  ReactClass props

-- | React class for components.
foreign import data ReactClass :: Type -> Type

-- | Read the component props.
foreign import getProps :: forall props state eff.
  ReactThis props state ->
  Eff (props :: ReactProps | eff) props

-- | Read the component children property.
foreign import getChildren :: forall props state eff.
  ReactThis props state ->
  Eff (props :: ReactProps | eff) (Array ReactElement)

-- | Write the component state.
foreign import writeState :: forall props state access eff.
  ReactThis props state ->
  state ->
  Eff (state :: ReactState (write :: Write | access) | eff) state

-- | Write the component state with a callback.
foreign import writeStateWithCallback :: forall props state access eff.
  ReactThis props state ->
  state ->
  Eff (state :: ReactState (write :: Write | access) | eff) Unit -> Eff (state :: ReactState (write :: Write | access) | eff) state

-- | Read the component state.
foreign import readState :: forall props state access eff.
  ReactThis props state ->
  Eff (state :: ReactState (read :: Read | access) | eff) state

-- | Transform the component state by applying a function.
foreign import transformState :: forall props state eff.
  ReactThis props state ->
  (state -> state) ->
  Eff (state :: ReactState ReadWrite | eff) Unit

-- | Force render of a react component.
forceUpdate :: forall eff props state.
  ReactThis props state -> Eff eff Unit
forceUpdate this = forceUpdateCb this (pure unit)

foreign import forceUpdateCbImpl :: forall eff e props state.
  EffFn2 eff
    (ReactThis props state)
    (Eff e Unit)
    Unit

-- | Force render and then run an Eff computation.
forceUpdateCb :: forall eff props state.
  ReactThis props state -> Eff eff Unit -> Eff eff Unit
forceUpdateCb this m = runEffFn2 forceUpdateCbImpl this m

-- | Create an event handler.
foreign import handle :: forall eff ev props state result.
  (ev -> EventHandlerContext eff props state result) -> EventHandler ev

-- | Create an element from a React class spreading the children array. Used when the children are known up front.
foreign import createElement :: forall props.
  ReactClass props -> props -> Array ReactElement -> ReactElement

-- | Create an element from a React class passing the children array. Used for a dynamic array of children.
foreign import createElementDynamic :: forall props.
  ReactClass props -> props -> Array ReactElement -> ReactElement

-- | Create an element from a tag name spreading the children array. Used when the children are known up front.
foreign import createElementTagName :: forall props.
  TagName -> props -> Array ReactElement -> ReactElement

-- | Create an element from a tag name passing the children array. Used for a dynamic array of children.
foreign import createElementTagNameDynamic :: forall props.
  TagName -> props -> Array ReactElement -> ReactElement

-- | Internal representation for the children elements passed to a component
foreign import data Children :: Type

-- | Internal conversion function from children elements to an array of React elements
foreign import childrenToArray :: Children -> Array ReactElement

foreign import preventDefault :: forall eff. Event -> Eff eff Unit

foreign import stopPropagation :: forall eff. Event -> Eff eff Unit
