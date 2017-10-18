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
  , ReactRefs

  , Refs
  , Ref

  , Render
  , GetInitialState
  , ComponentWillMount
  , ComponentDidMount
  , ComponentDidCatch
  , ComponentWillReceiveProps
  , ShouldComponentUpdate
  , ComponentWillUpdate
  , ComponentDidUpdate
  , ComponentWillUnmount

  , ReactSpec
  , ReactClass

  , Event
  , MouseEvent
  , KeyboardEvent

  , EventHandlerContext

  , spec, spec'

  , getProps
  , getRefs
  , readRef
  , writeRef
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

  , createClass
  , createClassStateless
  , createClassStateless'
  , createElement
  , createElementDynamic
  , createElementTagName
  , createElementTagNameDynamic
  , createFactory

  , Children
  , childrenToArray
  ) where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(Nothing))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Unsafe.Coerce (unsafeCoerce)

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

-- | This effect indicates that a computation may read the component refs.
-- |
-- | The first type argument is a row of access types (`Read`, `Write`).
foreign import data ReactRefs :: # Effect -> Effect

-- | The type of refs objects.
foreign import data Refs :: Type

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
    , refs :: ReactRefs ReadOnly
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
type Render props state render eff =
  ReactThis props state ->
  Eff
    ( props :: ReactProps
    , refs :: ReactRefs Disallowed
    , state :: ReactState ReadOnly
    | eff
    ) render

-- | A get initial state function.
type GetInitialState props state eff =
  ReactThis props state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState Disallowed
    , refs :: ReactRefs Disallowed
    | eff
    ) state

-- | A component will mount function.
type ComponentWillMount props state eff =
  ReactThis props state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs Disallowed
    | eff
    ) Unit

-- | A component did mount function.
type ComponentDidMount props state eff =
  ReactThis props state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

type ComponentDidCatch props state eff =
  ReactThis props state ->
  Error ->
  { componentStack :: String } ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit


-- | A component will receive props function.
type ComponentWillReceiveProps props state eff =
   ReactThis props state ->
   props ->
   Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A should component update function.
type ShouldComponentUpdate props state eff =
  ReactThis props state ->
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Boolean

-- | A component will update function.
type ComponentWillUpdate props state eff =
  ReactThis props state ->
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A component did update function.
type ComponentDidUpdate props state eff =
  ReactThis props state ->
  props ->
  state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A component will unmount function.
type ComponentWillUnmount props state eff =
  ReactThis props state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    , refs :: ReactRefs ReadOnly
    | eff
    ) Unit

-- | A specification of a component.
type ReactSpec props state render eff =
  { render :: Render props state render eff
  , displayName :: String
  , getInitialState :: GetInitialState props state eff
  , componentWillMount :: ComponentWillMount props state eff
  , componentDidMount :: ComponentDidMount props state eff
  , componentDidCatch :: Maybe (ComponentDidCatch props state eff)
  , componentWillReceiveProps :: ComponentWillReceiveProps props state eff
  , shouldComponentUpdate :: ShouldComponentUpdate props state eff
  , componentWillUpdate :: ComponentWillUpdate props state eff
  , componentDidUpdate :: ComponentDidUpdate props state eff
  , componentWillUnmount :: ComponentWillUnmount props state eff
  }

-- | Create a component specification with a provided state.
spec :: forall props state render eff.
  ReactRender render =>
  state -> Render props state render eff -> ReactSpec props state render eff
spec state = spec' \_ -> pure state

-- | Create a component specification with a get initial state function.
spec' :: forall props state render eff.
  ReactRender render =>
  GetInitialState props state eff ->
  Render props state render eff ->
  ReactSpec props state render eff
spec' getInitialState renderFn =
  { render: renderFn
  , displayName: ""
  , getInitialState: getInitialState
  , componentWillMount: \_ -> pure unit
  , componentDidMount: \_ -> pure unit
  , componentDidCatch: Nothing
  , componentWillReceiveProps: \_ _ -> pure unit
  , shouldComponentUpdate: \_ _ _ -> pure true
  , componentWillUpdate: \_ _ _ -> pure unit
  , componentDidUpdate: \_ _ _ -> pure unit
  , componentWillUnmount: \_ -> pure unit
  }

-- | React class for components.
foreign import data ReactClass :: Type -> Type

-- | Read the component props.
foreign import getProps :: forall props state eff.
  ReactThis props state ->
  Eff (props :: ReactProps | eff) props

-- | Read the component refs.
foreign import getRefs :: forall props state access eff.
  ReactThis props state ->
  Eff (refs :: ReactRefs (read :: Read | access) | eff) Refs

-- | Ref type.  You can store `Ref` types on `Refs` object (which in
-- | corresponds to `this.refs`).  Use `ReactDOM.refToNode` if you want to
-- | store a `DOM.Node.Types.Node`
foreign import data Ref :: Type

foreign import readRefImpl :: forall props state access eff.
  ReactThis props state ->
  String ->
  Eff (refs :: ReactRefs (read :: Read | access) | eff) (Nullable Ref)

-- | Read named ref from `Refs`.
readRef :: forall props state access eff.
  ReactThis props state ->
  String ->
  Eff (refs :: ReactRefs (read :: Read | access) | eff) (Maybe Ref)
readRef this name = toMaybe <$> readRefImpl this name

-- | Write a `Ref` to `Refs`
foreign import writeRef :: forall props state access eff.
  ReactThis props state ->
  String ->
  Nullable Ref ->
  Eff (refs :: ReactRefs (write :: Write | access) | eff) Unit

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
foreign import writeStateWithCallback :: forall props state access eff. ReactThis props state -> state -> Eff (state :: ReactState (write :: Write | access) | eff) Unit -> Eff (state :: ReactState (write :: Write | access) | eff) state

-- | Read the component state.
foreign import readState :: forall props state access eff.
  ReactThis props state ->
  Eff (state :: ReactState (read :: Read | access) | eff) state

-- | Transform the component state by applying a function.
foreign import transformState :: forall props state eff.
  ReactThis props state ->
  (state -> state) ->
  Eff (state :: ReactState ReadWrite | eff) Unit

-- | Create a React class from a specification.
foreign import createClass' :: forall props state render eff.
  Fn2
    (forall a. Maybe a -> Nullable a)
    (ReactSpec props state render eff)
    (ReactClass props)

createClass :: forall props state render eff.
  ReactSpec props state render eff -> ReactClass props
createClass spc = runFn2 createClass' toNullable spc

-- | Create a stateless React class.  When using a non anonymous function the
-- | displayName will be the capitalized name of the function, e.g.
-- | ``` purescript
-- | helloWorld = createClassStatelesss hellowWorldCls
-- |    where
-- |      hellowWorldCls props = ...
-- | ```
-- | Then the `displayName` will be set up to `HellowWorldCls`
foreign import createClassStateless :: forall props render.
  ReactRender render =>
  (props -> render) -> ReactClass props

-- | Create a stateless React class with children access.
createClassStateless' :: forall props render.
  ReactRender render =>
  (props -> Array ReactElement -> render) -> ReactClass props
createClassStateless' k =
  createClassStateless \props ->
    k props (childrenToArray (unsafeCoerce props).children)

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

-- | Create a factory from a React class.
foreign import createFactory :: forall props.
  ReactClass props -> props -> ReactElement

-- | Internal representation for the children elements passed to a component
foreign import data Children :: Type

-- | Internal conversion function from children elements to an array of React elements
foreign import childrenToArray :: Children -> Array ReactElement

foreign import preventDefault :: forall eff a. Event -> Eff eff a

foreign import stopPropagation :: forall eff a. Event -> Eff eff a
