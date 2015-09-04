## Module React

This module defines foreign types and functions which wrap React's functionality.

#### `ReactElement`

``` purescript
data ReactElement :: *
```

A virtual DOM node, or component.

#### `ReactThis`

``` purescript
data ReactThis :: * -> * -> *
```

A reference to a component, essentially React's `this`.

#### `EventHandler`

``` purescript
data EventHandler :: * -> *
```

An event handler. The type argument represents the type of the event.

#### `Disallowed`

``` purescript
data Disallowed
```

This phantom type indicates that both read and write access to a resource are disallowed.

#### `Read`

``` purescript
data Read write
```

This phantom type indicates that read access to a resource is allowed.

#### `Write`

``` purescript
data Write
```

This phantom type indicates that write access to a resource is allowed.

#### `Only`

``` purescript
data Only
```

This phantom type indicates that only read access to a resource is allowed.

#### `ReadWrite`

``` purescript
type ReadWrite = Read Write
```

An access synonym which indicates that both read and write access are allowed.

#### `ReadOnly`

``` purescript
type ReadOnly = Read Only
```

An access synonym which indicates that reads are allowed but writes are not.

#### `ReactState`

``` purescript
data ReactState :: * -> * -> !
```

This effect indicates that a computation may read or write the component state.

The first type argument is either `ReadWrite`, `ReadOnly` or `Disallowed` dependeding on the context.

The second type argument is the type of the state of the component.

#### `ReactProps`

``` purescript
data ReactProps :: * -> !
```

This effect indicates that a computation may read the component props.

#### `ReactRefs`

``` purescript
data ReactRefs :: * -> !
```

This effect indicates that a computation may read the component refs.

The first type argument is either `ReadOnly` or `Disallowed` dependeding on the context.

#### `Refs`

``` purescript
data Refs :: *
```

The type of refs objects.

#### `Event`

``` purescript
data Event :: *
```

The type of DOM events.

#### `MouseEvent`

``` purescript
type MouseEvent = { pageX :: Number, pageY :: Number }
```

The type of mouse events.

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = { altKey :: Boolean, ctrlKey :: Boolean, charCode :: Int, key :: String, keyCode :: Int, locale :: String, location :: Int, metaKey :: Boolean, repeat :: Boolean, shiftKey :: Boolean, which :: Int }
```

The type of keyboard events.

#### `EventHandlerContext`

``` purescript
type EventHandlerContext eff props state result = Eff (props :: ReactProps props, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite state | eff) result
```

A function which handles events.

#### `Render`

``` purescript
type Render props state eff = ReactThis props state -> Eff (props :: ReactProps props, refs :: ReactRefs Disallowed, state :: ReactState ReadOnly state | eff) ReactElement
```

A rendering function.

#### `ReactSpec`

``` purescript
type ReactSpec props state eff = { render :: Render props state eff, displayName :: String, getInitialState :: ReactThis props state -> Eff (props :: ReactProps props, state :: ReactState Disallowed state, refs :: ReactRefs Disallowed | eff) state, componentWillMount :: ReactThis props state -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs Disallowed | eff) Unit, componentDidMount :: ReactThis props state -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Unit, componentWillReceiveProps :: ReactThis props state -> props -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Unit, shouldComponentUpdate :: ReactThis props state -> props -> state -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Boolean, componentWillUpdate :: ReactThis props state -> props -> state -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Unit, componentDidUpdate :: ReactThis props state -> props -> state -> Eff (props :: ReactProps props, state :: ReactState ReadOnly state, refs :: ReactRefs ReadOnly | eff) Unit, componentWillUnmount :: ReactThis props state -> Eff (props :: ReactProps props, state :: ReactState ReadOnly state, refs :: ReactRefs ReadOnly | eff) Unit }
```

A specification of a component.

#### `spec`

``` purescript
spec :: forall props state eff. state -> Render props state eff -> ReactSpec props state eff
```

Create a component specification.

#### `ReactClass`

``` purescript
data ReactClass :: * -> *
```

React class for components.

#### `getProps`

``` purescript
getProps :: forall props state eff. ReactThis props state -> Eff (props :: ReactProps props | eff) props
```

Read the component props.

#### `getRefs`

``` purescript
getRefs :: forall props state write eff. ReactThis props state -> Eff (refs :: ReactRefs (Read write) | eff) Refs
```

Read the component refs.

#### `getChildren`

``` purescript
getChildren :: forall props state eff. ReactThis props state -> Eff (props :: ReactProps props | eff) (Array ReactElement)
```

Read the component children property.

#### `writeState`

``` purescript
writeState :: forall props state eff. ReactThis props state -> state -> Eff (state :: ReactState ReadWrite state | eff) state
```

Write the component state.

#### `readState`

``` purescript
readState :: forall props state write eff. ReactThis props state -> Eff (state :: ReactState (Read write) state | eff) state
```

Read the component state.

#### `transformState`

``` purescript
transformState :: forall props state eff. ReactThis props state -> (state -> state) -> Eff (state :: ReactState ReadWrite state | eff) state
```

Transform the component state by applying a function.

#### `createClass`

``` purescript
createClass :: forall props state eff. ReactSpec props state eff -> ReactClass props
```

Create a React class from a specification.

#### `handle`

``` purescript
handle :: forall eff ev props state result. (ev -> EventHandlerContext eff props state result) -> EventHandler ev
```

Create an event handler.

#### `createElement`

``` purescript
createElement :: forall props. ReactClass props -> props -> Array ReactElement -> ReactElement
```

Create an element from a React class.

#### `createFactory`

``` purescript
createFactory :: forall props. ReactClass props -> props -> ReactElement
```

Create a factory from a React class.

#### `render`

``` purescript
render :: forall eff. ReactElement -> Element -> Eff (dom :: DOM | eff) ReactElement
```

Render a React element in a document element.

#### `renderToString`

``` purescript
renderToString :: ReactElement -> String
```

Render a React element as a string.


