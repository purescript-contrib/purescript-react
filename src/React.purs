-- | This module defines foreign types and functions which wrap React's functionality.

module React
  ( ReactElement
  , ReactComponent
  , ReactThis
  , TagName

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
  , getState
  , setState
  , setStateWithCallback
  , writeState
  , writeStateWithCallback
  , transformState

  , forceUpdate
  , forceUpdateWithCallback

  , createElement
  , createElementDynamic
  , createElementTagName
  , createElementTagNameDynamic
  , createLeafElement

  , SyntheticEventHandler

  , Children
  , childrenToArray
  , childrenCount

  , class ReactPropFields
  , class IsReactElement
  , toElement
  , fragmentWithKey
  ) where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn2)
import Prim.Row as Row
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | Name of a tag.
type TagName = String

-- | A virtual DOM node, or component.
foreign import data ReactElement :: Type

instance semigroupReactElement :: Semigroup ReactElement where
  append a b = toElement [ a, b ]

instance monoidReactElement :: Monoid ReactElement where
  mempty = toElement ([] :: Array ReactElement)

-- | A mounted react component
foreign import data ReactComponent :: Type

-- | A reference to a component, essentially React's `this`.
foreign import data ReactThis :: Type -> Type -> Type

type SyntheticEventHandler event = EffectFn1 event Unit

-- | A render effect.
type Render = Effect ReactElement

-- | A component will mount effect.
type ComponentWillMount = Effect Unit

-- | A component did mount effect.
type ComponentDidMount = Effect Unit

-- | A component did catch effect.
type ComponentDidCatch = Error -> { componentStack :: String } -> Effect Unit

-- | A component will receive props function.
type ComponentWillReceiveProps props = props -> Effect Unit

-- | A should component update function.
type ShouldComponentUpdate props state = props -> state -> Effect Boolean

-- | A component will update function.
type ComponentWillUpdate props state = props -> state -> Effect Unit

-- | A component did update function.
type ComponentDidUpdate props state = props -> state -> Effect Unit

-- | A component will unmount effect..
type ComponentWillUnmount = Effect Unit

-- | Required fields for constructing a ReactClass.
type ReactSpecRequired state r =
  ( state :: state
  , render :: Render
  | r
  )

-- | Optional fields for constructing a ReactClass.
type ReactSpecOptional props state r =
  ( componentWillMount :: ComponentWillMount
  , componentDidMount :: ComponentDidMount
  , componentDidCatch :: ComponentDidCatch
  , componentWillReceiveProps :: ComponentWillReceiveProps props
  , componentWillUpdate :: ComponentWillUpdate props state
  , componentDidUpdate :: ComponentDidUpdate props state
  , componentWillUnmount :: ComponentWillUnmount
  | r
  )

type ReactSpecShouldComponentUpdate props state =
  ( shouldComponentUpdate :: ShouldComponentUpdate props state
  )

type ReactSpecAll props state
  = ReactSpecRequired state
  + ReactSpecOptional props state
  + ReactSpecShouldComponentUpdate props state

type ReactSpecPure props state
  = ReactSpecRequired state
  + ReactSpecOptional props state ()

-- | The signature for a ReactClass constructor. A constructor takes the
-- | `ReactThis` context and returns a record with appropriate lifecycle
-- | methods.
type ReactClassConstructor props state r =
  ReactThis props state ->
  Effect (Record (ReactSpecRequired state r))

-- | Creates a `ReactClass`` inherited from `React.Component`.
component
  :: forall props state r x
   . Row.Union (ReactSpecRequired (Record state) r) x (ReactSpecAll (Record props) (Record state))
  => String
  -> ReactClassConstructor (Record props) (Record state) r
  -> ReactClass (Record props)
component = componentImpl

-- | Creates a `ReactClass`` inherited from `React.PureComponent`.
pureComponent
  :: forall props state r x
   . Row.Union (ReactSpecRequired (Record state) r) x (ReactSpecPure (Record props) (Record state))
  => String
  -> ReactClassConstructor (Record props) (Record state) r
  -> ReactClass (Record props)
pureComponent = pureComponentImpl

foreign import componentImpl :: forall this props r.
  String ->
  (this -> Effect r) ->
  ReactClass props

foreign import pureComponentImpl :: forall this props r.
  String ->
  (this -> Effect r) ->
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
foreign import getProps :: forall props state.
  ReactThis props state ->
  Effect props

-- | Write the component state.
foreign import setStateImpl :: forall props given all.
  ReactThis props (Record all) ->
  Record given ->
  Effect Unit

-- | Write the component state with a callback.
foreign import setStateWithCallbackImpl :: forall props given all.
  ReactThis props (Record all) ->
  Record given ->
  Effect Unit ->
  Effect Unit

-- | Get the component state.
foreign import getState :: forall props state.
  ReactThis props state ->
  Effect state

-- | Transform the component state by applying a function.
foreign import transformState :: forall props state.
  ReactThis props state ->
  (state -> state) ->
  Effect Unit

setState :: forall props given rest all.
  Row.Union given rest all =>
  ReactThis props (Record all) ->
  Record given ->
  Effect Unit
setState = setStateImpl

setStateWithCallback :: forall props given rest all.
  Row.Union given rest all =>
  ReactThis props (Record all) ->
  Record given ->
  Effect Unit ->
  Effect Unit
setStateWithCallback = setStateWithCallbackImpl

writeState :: forall props all.
  ReactThis props (Record all) ->
  Record all ->
  Effect Unit
writeState = setStateImpl

writeStateWithCallback :: forall props all.
  ReactThis props (Record all) ->
  Record all ->
  Effect Unit ->
  Effect Unit
writeStateWithCallback = setStateWithCallbackImpl

-- | Force render of a react component.
forceUpdate :: forall props state. ReactThis props state -> Effect Unit
forceUpdate this = forceUpdateWithCallback this (pure unit)

foreign import forceUpdateCbImpl :: forall props state.
  EffectFn2
    (ReactThis props state)
    (Effect Unit)
    Unit

-- | Force render and then run an Effect.
forceUpdateWithCallback :: forall props state. ReactThis props state -> Effect Unit -> Effect Unit
forceUpdateWithCallback this m = runEffectFn2 forceUpdateCbImpl this m

class ReactPropFields (required :: # Type) (given :: # Type)

type ReservedReactPropFields r =
  ( key :: String
  , ref :: SyntheticEventHandler (Nullable Ref)
  | r
  )

instance reactPropFields ::
  ( Row.Union given optional (ReservedReactPropFields required)
  , Row.Union optional leftover (ReservedReactPropFields ())
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
  toElement = identity

instance isReactElementArray :: IsReactElement (Array ReactElement) where
  toElement = createElement fragment {}

-- | Creates a keyed fragment.
fragmentWithKey :: String -> Array ReactElement -> ReactElement
fragmentWithKey = createElement fragment <<< { key: _ }
