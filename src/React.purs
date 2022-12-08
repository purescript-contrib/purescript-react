-- | This module defines foreign types and functions which wrap React's functionality.

module React
  ( TagName
  , ReactElement
  , ReactComponent
  , ReactThis
  , ReactUnusedSnapshot
  , SyntheticEventHandler
  , Render
  , ComponentWillMount
  , ComponentDidMount
  , ComponentDidCatch
  , ComponentWillReceiveProps
  , ShouldComponentUpdate
  , ComponentWillUpdate
  , ComponentDidUpdate
  , GetSnapshotBeforeUpdate
  , ComponentWillUnmount
  , ReactSpecRequired
  , ReactSpecUnsafe
  , ReactSpecOptional
  , ReactSpecShouldComponentUpdate
  , ReactSpecAll
  , ReactSpecPure
  , ReactClassConstructor
  , class ReactComponentSpec
  , class ReactPureComponentSpec
  , component
  , componentWithDerivedState
  , pureComponent
  , pureComponentWithDerivedState
  , statelessComponent
  , ReactClass
  , getProps
  , getState
  , setState
  , setStateWithCallback
  , writeState
  , writeStateWithCallback
  , modifyState
  , modifyStateWithCallback
  , forceUpdate
  , forceUpdateWithCallback
  , class ReactPropFields
  , ReservedReactPropFields
  , createElement
  , unsafeCreateElement
  , createElementDynamic
  , unsafeCreateElementDynamic
  , createLeafElement
  , unsafeCreateLeafElement
  , createElementTagName
  , createElementTagNameDynamic
  , Children
  , childrenToArray
  , childrenCount
  , class IsReactElement
  , toElement
  , fragment
  , fragmentWithKey
  , Context
  , ContextProvider
  , ContextConsumer
  , createContext
  ) where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1)
import Prim.Row as Row
import React.Ref as Ref
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | Name of a tag.
type TagName = String

-- | A virtual DOM node, or component.
foreign import data ReactElement :: Type

foreign import emptyReactElement :: ReactElement

foreign import isEmptyReactElement :: ReactElement -> Boolean

instance semigroupReactElement :: Semigroup ReactElement where
  append a b
    | isEmptyReactElement a = b
    | isEmptyReactElement b = a
    | otherwise = toElement [ a, b ]

instance monoidReactElement :: Monoid ReactElement where
  mempty = emptyReactElement

-- | A mounted react component
foreign import data ReactComponent :: Type

-- | A reference to a component, essentially React's `this`.
foreign import data ReactThis :: Type -> Type -> Type

type role ReactThis representational representational

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

type ReactSpecAll props state snapshot =
  ReactSpecRequired state
    + ReactSpecOptional props state snapshot
    + ReactSpecShouldComponentUpdate props state

type ReactSpecPure props state snapshot =
  ReactSpecRequired state
    + ReactSpecOptional props state snapshot ()

-- | The signature for a ReactClass constructor. A constructor takes the
-- | `ReactThis` context and returns a record with appropriate lifecycle
-- | methods.
type ReactClassConstructor props state r = ReactThis props state -> Effect (Record r)

class ReactComponentSpec :: Type -> Type -> Type -> Row Type -> Row Type -> Constraint
class ReactComponentSpec props state snapshot (given :: Row Type) (spec :: Row Type)

instance reactComponentSpec ::
  ( Row.Union given (ReactSpecAll props state ReactUnusedSnapshot) spec
  , Row.Nub spec (ReactSpecAll props state snapshot)
  ) =>
  ReactComponentSpec props state snapshot given spec

class ReactPureComponentSpec :: Type -> Type -> Type -> Row Type -> Row Type -> Constraint
class ReactPureComponentSpec props state snapshot (given :: Row Type) (spec :: Row Type)

instance reactPureComponentSpec ::
  ( Row.Union given (ReactSpecPure props state ReactUnusedSnapshot) spec
  , Row.Nub spec (ReactSpecPure props state snapshot)
  ) =>
  ReactPureComponentSpec props state snapshot given spec

-- | Creates a `ReactClass` inherited from `React.Component`.
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

-- | Creates a `ReactClass` inherited from `React.PureComponent`.
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

foreign import componentImpl
  :: forall this props r
   . String
  -> (this -> Effect r)
  -> ReactClass props

foreign import componentWithDerivedStateImpl
  :: forall this props state r
   . String
  -> (props -> state -> state)
  -> (this -> Effect r)
  -> ReactClass props

foreign import pureComponentImpl
  :: forall this props r
   . String
  -> (this -> Effect r)
  -> ReactClass props

foreign import pureComponentWithDerivedStateImpl
  :: forall this props state r
   . String
  -> (props -> state -> state)
  -> (this -> Effect r)
  -> ReactClass props

foreign import statelessComponent
  :: forall props
   . (Record props -> ReactElement)
  -> ReactClass (Record props)

-- | React class for components.
foreign import data ReactClass :: Type -> Type

type role ReactClass representational

foreign import fragment :: ReactClass { children :: Children }

-- | Read the component props.
foreign import getProps
  :: forall props state
   . ReactThis props state
  -> Effect props

foreign import setStateImpl
  :: forall props state update
   . ReactThis props state
  -> update
  -> Effect Unit

foreign import setStateWithCallbackImpl
  :: forall props state update
   . ReactThis props state
  -> update
  -> Effect Unit
  -> Effect Unit

-- | Get the component state.
foreign import getState
  :: forall props state
   . ReactThis props state
  -> Effect state

-- | Update component state given some sub-set of state properties.
setState
  :: forall props given rest all
   . Row.Union given rest all
  => ReactThis props (Record all)
  -> Record given
  -> Effect Unit
setState = setStateImpl

-- | Update component state given some sub-set of state properties, while
-- | also invoking a callback when applied.
setStateWithCallback
  :: forall props given rest all
   . Row.Union given rest all
  => ReactThis props (Record all)
  -> Record given
  -> Effect Unit
  -> Effect Unit
setStateWithCallback = setStateWithCallbackImpl

-- | Update component state.
writeState
  :: forall props all
   . ReactThis props (Record all)
  -> Record all
  -> Effect Unit
writeState = setStateImpl

-- | Update component state, while also invoking a callback when applied.
writeStateWithCallback
  :: forall props all
   . ReactThis props (Record all)
  -> Record all
  -> Effect Unit
  -> Effect Unit
writeStateWithCallback = setStateWithCallbackImpl

-- | Update component state given a modification function.
modifyState
  :: forall props state
   . ReactThis props state
  -> (state -> state)
  -> Effect Unit
modifyState = setStateImpl

-- | Update component state given a modification function, while also invoking
-- | a callback when applied.
modifyStateWithCallback
  :: forall props state
   . ReactThis props state
  -> (state -> state)
  -> Effect Unit
  -> Effect Unit
modifyStateWithCallback = setStateWithCallbackImpl

-- | Force render of a react component.
forceUpdate :: forall props state. ReactThis props state -> Effect Unit
forceUpdate this = forceUpdateWithCallback this (pure unit)

-- | Force render and then run an Effect.
foreign import forceUpdateWithCallback
  :: forall props state
   . ReactThis props state
  -> Effect Unit
  -> Effect Unit

class ReactPropFields (required :: Row Type) (given :: Row Type)

type ReservedReactPropFields r =
  ( key :: String
  , ref :: Ref.RefHandler Ref.ReactInstance
  | r
  )

instance reactPropFields ::
  ( Row.Union given optional (ReservedReactPropFields required)
  , Row.Union optional leftover (ReservedReactPropFields ())
  ) =>
  ReactPropFields required given

-- | Create an element from a React class spreading the children array. Used when the children are known up front.
createElement
  :: forall required given
   . ReactPropFields required given
  => ReactClass { children :: Children | required }
  -> { | given }
  -> Array ReactElement
  -> ReactElement
createElement = createElementImpl

-- | An unsafe version of `createElement` which does not enforce the reserved
-- | properties "key" and "ref".
unsafeCreateElement
  :: forall props
   . ReactClass { children :: Children | props }
  -> { | props }
  -> Array ReactElement
  -> ReactElement
unsafeCreateElement = createElementImpl

-- | Create an element from a React class passing the children array. Used for a dynamic array of children.
createElementDynamic
  :: forall required given
   . ReactPropFields required given
  => ReactClass { children :: Children | required }
  -> { | given }
  -> Array ReactElement
  -> ReactElement
createElementDynamic = createElementDynamicImpl

-- | An unsafe version of `createElementDynamic` which does not enforce the reserved
-- | properties "key" and "ref".
unsafeCreateElementDynamic
  :: forall props
   . ReactClass { children :: Children | props }
  -> { | props }
  -> Array ReactElement
  -> ReactElement
unsafeCreateElementDynamic = createElementDynamicImpl

foreign import createElementImpl
  :: forall required given children
   . ReactClass required
  -> given
  -> Array children
  -> ReactElement

foreign import createElementDynamicImpl
  :: forall required given children
   . ReactClass required
  -> given
  -> Array children
  -> ReactElement

-- | Create an element from a React class that does not require children. Additionally it can be used
-- | when the children are represented /only/ through the `children` prop - for instance, a `ContextConsumer`
-- | would be turned into a `ReactElement` with `createLeafElement someContext.consumer { children: \x -> ... }`.
createLeafElement
  :: forall required given
   . ReactPropFields required given
  => ReactClass { | required }
  -> { | given }
  -> ReactElement
createLeafElement = createLeafElementImpl

-- | An unsafe version of `createLeafElement` which does not enforce the reserved
-- | properties "key" and "ref".
unsafeCreateLeafElement
  :: forall props
   . ReactClass props
  -> props
  -> ReactElement
unsafeCreateLeafElement = createLeafElementImpl

foreign import createLeafElementImpl
  :: forall required given
   . ReactClass required
  -> given
  -> ReactElement

-- | Create an element from a tag name spreading the children array. Used when the children are known up front.
foreign import createElementTagName
  :: forall props
   . TagName
  -> props
  -> Array ReactElement
  -> ReactElement

-- | Create an element from a tag name passing the children array. Used for a dynamic array of children.
foreign import createElementTagNameDynamic
  :: forall props
   . TagName
  -> props
  -> Array ReactElement
  -> ReactElement

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
  toElement = case _ of
    [] -> mempty
    children -> createElement fragment {} children

-- | Creates a keyed fragment.
fragmentWithKey :: String -> Array ReactElement -> ReactElement
fragmentWithKey = createElement fragment <<< { key: _ }

type Context a =
  { consumer :: ContextConsumer a
  , provider :: ContextProvider a
  }

type ContextProvider a = ReactClass { children :: Children, value :: a }

type ContextConsumer a = ReactClass { children :: a -> ReactElement }

-- | Create a new context provider/consumer pair given a default value.
foreign import createContext :: forall a. a -> Context a
