-- | This module defines foreign types and functions which wrap React's functionality.

module React
  ( ReactElement
  , ReactComponent
  , ReactThis
  , ReactUnusedSnapshot
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

  , class ReactComponentSpec
  , class ReactPureComponentSpec
  , component
  , componentWithDerivedState
  , pureComponent
  , pureComponentWithDerivedState
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

  , ContextProvider
  , ContextConsumer
  , createContext
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

foreign import data ReactUnusedSnapshot :: Type

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
type ComponentDidUpdate props state snapshot = props -> state -> snapshot -> Effect Unit

type GetSnapshotBeforeUpdate props state snapshot = props -> state -> Effect snapshot

-- | A component will unmount effect.
type ComponentWillUnmount = Effect Unit

-- | Required fields for constructing a ReactClass.
type ReactSpecRequired state r =
  ( state :: state
  , render :: Render
  | r
  )

type ReactSpecUnsafe props state r =
  ( unsafeComponentWillMount :: ComponentWillMount
  , unsafeComponentWillReceiveProps :: ComponentWillReceiveProps props
  , unsafeComponentWillUpdate :: ComponentWillUpdate props state
  | r
  )

-- | Optional fields for constructing a ReactClass.
type ReactSpecOptional props state snapshot r =
  ( componentDidMount :: ComponentDidMount
  , componentDidCatch :: ComponentDidCatch
  , componentWillUnmount :: ComponentWillUnmount
  , componentDidUpdate :: ComponentDidUpdate props state snapshot
  , getSnapshotBeforeUpdate :: GetSnapshotBeforeUpdate props state snapshot
  | ReactSpecUnsafe props state r
  )

type ReactSpecShouldComponentUpdate props state =
  ( shouldComponentUpdate :: ShouldComponentUpdate props state
  )

type ReactSpecAll props state snapshot
  = ReactSpecRequired state
  + ReactSpecOptional props state snapshot
  + ReactSpecShouldComponentUpdate props state

type ReactSpecPure props state snapshot
  = ReactSpecRequired state
  + ReactSpecOptional props state snapshot ()

-- | The signature for a ReactClass constructor. A constructor takes the
-- | `ReactThis` context and returns a record with appropriate lifecycle
-- | methods.
type ReactClassConstructor props state r =
  ReactThis props state ->
  Effect (Record r)

class ReactComponentSpec props state snapshot (given :: # Type) (spec :: # Type)

instance reactComponentSpec ::
  ( Row.Union
      (ReactSpecRequired state given)
      (ReactSpecAll props state ReactUnusedSnapshot)
      spec
  , Row.Nub spec (ReactSpecAll props state snapshot)
  ) =>
  ReactComponentSpec props state snapshot given spec

class ReactPureComponentSpec props state snapshot (given :: # Type) (spec :: # Type)

instance reactPureComponentSpec ::
  ( Row.Union
      (ReactSpecRequired state given)
      (ReactSpecPure props state ReactUnusedSnapshot)
      spec
  , Row.Nub spec (ReactSpecAll props state snapshot)
  ) =>
  ReactPureComponentSpec props state snapshot given spec

-- | Creates a `ReactClass`` inherited from `React.Component`.
component
  :: forall props state snapshot given spec
   . ReactComponentSpec (Record props) (Record state) snapshot given spec
  => String
  -> ReactClassConstructor (Record props) (Record state) given
  -> ReactClass (Record props)
component = componentImpl

-- | Like `component`, but takes a `getDerivedStateFromProps` handler.
componentWithDerivedState
  :: forall props state snapshot given spec
   . ReactComponentSpec (Record props) (Record state) snapshot given spec
  => String
  -> (Record props -> Record state -> Record state)
  -> ReactClassConstructor (Record props) (Record state) given
  -> ReactClass (Record props)
componentWithDerivedState = componentWithDerivedStateImpl

-- | Creates a `ReactClass`` inherited from `React.PureComponent`.
pureComponent
  :: forall props state snapshot given spec
   . ReactPureComponentSpec (Record props) (Record state) snapshot given spec
  => String
  -> ReactClassConstructor (Record props) (Record state) given
  -> ReactClass (Record props)
pureComponent = pureComponentImpl

-- | Like `pureComponent`, but takes a `getDerivedStateFromProps` handler.
pureComponentWithDerivedState
  :: forall props state snapshot given spec
   . ReactPureComponentSpec (Record props) (Record state) snapshot given spec
  => String
  -> (Record props -> Record state -> Record state)
  -> ReactClassConstructor (Record props) (Record state) given
  -> ReactClass (Record props)
pureComponentWithDerivedState = componentWithDerivedStateImpl

foreign import componentImpl :: forall this props r.
  String ->
  (this -> Effect r) ->
  ReactClass props

foreign import componentWithDerivedStateImpl :: forall this props state r.
  String ->
  (props -> state -> state) ->
  (this -> Effect r) ->
  ReactClass props

foreign import pureComponentImpl :: forall this props r.
  String ->
  (this -> Effect r) ->
  ReactClass props

foreign import pureComponentWithDerivedStateImpl :: forall this props state r.
  String ->
  (props -> state -> state) ->
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

type ContextProvider a = ReactClass { children :: Children, value :: a }

type ContextConsumer a = ReactClass { children :: a -> ReactElement }

foreign import createContext :: forall a.
  a ->
  { consumer :: ContextConsumer a
  , provider :: ContextProvider a
  }
