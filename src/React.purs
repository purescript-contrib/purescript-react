-- | This module defines foreign types and functions which wrap React's functionality.

module React
  ( ReactElement
  , ReactComponent
  , ReactThis
  , TagName

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
  , Ref

  , component
  , pureComponent
  , statelessComponent

  , getProps
  , readState
  , writeState
  , writeStateWithCallback
  , transformState

  , forceUpdate
  , forceUpdateCb

  , createElement
  , createElementDynamic
  , createElementTagName
  , createElementTagNameDynamic
  , createLeafElement

  , SyntheticEventHandler
  , SyntheticEventHandlerContext
  , handle

  , Children
  , childrenToArray
  , childrenCount

  , class ReactPropFields
  , class IsReactElement
  , toElement
  , fragmentWithKey

  , module SyntheticEvent
  ) where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)

import Data.Nullable (Nullable)

import React.SyntheticEvent as SyntheticEvent

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
foreign import data SyntheticEventHandler :: Type -> Type

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

-- | A function which handles events.
type SyntheticEventHandlerContext eff props state result =
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadWrite
    | eff
    ) result

-- | A render function.
type Render eff =
  Eff
    ( props :: ReactProps
    , state :: ReactState ReadOnly
    | eff
    ) ReactElement

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
type ReactSpecRequired state eff r =
  ( state :: state
  , render :: Render eff
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

type ReactSpecAll props state eff =
  ReactSpecRequired state eff (ReactSpecOptional props state eff (ReactSpecShouldComponentUpdate props state eff))

type ReactSpecPure props state eff =
  ReactSpecRequired state eff (ReactSpecOptional props state eff ())

-- | The signature for a ReactClass constructor. A constructor takes the
-- | `ReactThis` context and returns a record with appropriate lifecycle
-- | methods.
type ReactClassConstructor props state eff r =
  ReactThis props state ->
  Eff
    ( props :: ReactProps
    , state :: ReactState Disallowed
    | eff
    ) (Record (ReactSpecRequired state eff r))

-- | Creates a `ReactClass`` inherited from `React.Component`.
component
  :: forall props state eff r x
   . Union (ReactSpecRequired (Record state) eff r) x (ReactSpecAll (Record props) (Record state) eff)
  => String
  -> ReactClassConstructor (Record props) (Record state) eff r
  -> ReactClass (Record props)
component = componentImpl

-- | Creates a `ReactClass`` inherited from `React.PureComponent`.
pureComponent
  :: forall props state eff r x
   . Union (ReactSpecRequired (Record state) eff r) x (ReactSpecPure (Record props) (Record state) eff)
  => String
  -> ReactClassConstructor (Record props) (Record state) eff r
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

foreign import statelessComponent :: forall props.
  (Record props -> ReactElement) ->
  ReactClass (Record props)

-- | React class for components.
foreign import data ReactClass :: Type -> Type

foreign import fragment :: ReactClass { children :: Children }

-- | Type for React refs. This type is opaque, but you can use `Data.Foreign`
-- | and `DOM` to validate the underlying representation.
foreign import data Ref :: Type

-- | Read the component props.
foreign import getProps :: forall props state eff.
  ReactThis props state ->
  Eff (props :: ReactProps | eff) props

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
  (ev -> SyntheticEventHandlerContext eff props state result) -> SyntheticEventHandler ev

class ReactPropFields (required :: # Type) (given :: # Type)

type ReservedReactPropFields r =
  ( key :: String
  , ref :: SyntheticEventHandler (Nullable Ref)
  | r
  )

instance reactPropFields ::
  ( Union given optional (ReservedReactPropFields required)
  , Union optional leftover (ReservedReactPropFields ())
  ) =>
  ReactPropFields required given

-- | Create an element from a React class spreading the children array. Used when the children are known up front.
createElement :: forall required given.
  ReactPropFields required given =>
  ReactClass { children :: Children | required } ->
  { | given } ->
  Array ReactElement ->
  ReactElement
createElement = createElementImpl

-- | Create an element from a React class passing the children array. Used for a dynamic array of children.
createElementDynamic :: forall required given.
  ReactPropFields required given =>
  ReactClass { children :: Children | required } ->
  { | given } ->
  Array ReactElement ->
  ReactElement
createElementDynamic = createElementDynamicImpl

foreign import createElementImpl :: forall required given children.
  ReactClass required -> given -> Array children -> ReactElement

foreign import createElementDynamicImpl :: forall required given children.
  ReactClass required -> given -> Array children -> ReactElement

-- | Create an element from a React class that does not require children.
createLeafElement :: forall required given.
  ReactPropFields required given =>
  ReactClass { | required } ->
  { | given } ->
  ReactElement
createLeafElement = createLeafElementImpl

foreign import createLeafElementImpl :: forall required given.
  ReactClass required -> given -> ReactElement

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

-- | Returns the number of children.
foreign import childrenCount :: Children -> Int

class IsReactElement a where
  toElement :: a -> ReactElement

instance isReactElementString :: IsReactElement String where
  toElement = unsafeCoerce

instance isReactElementNumber :: IsReactElement Number where
  toElement = unsafeCoerce

instance isReactElementInt :: IsReactElement Int where
  toElement = unsafeCoerce

instance isReactElementChildren :: IsReactElement Children where
  toElement = unsafeCoerce

instance isReactElementReactElement :: IsReactElement ReactElement where
  toElement = id

instance isReactElementArray :: IsReactElement (Array ReactElement) where
  toElement = createElement fragment {}

-- | Creates a keyed fragment.
fragmentWithKey :: String -> Array ReactElement -> ReactElement
fragmentWithKey = createElement fragment <<< { key: _ }
