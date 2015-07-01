## Module React

This module defines foreign types and functions which wrap React's functionality.

#### `UI`

``` purescript
data UI :: *
```

A virtual DOM node, or component.

#### `UIRef`

``` purescript
data UIRef :: *
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
type Render props state eff = UIRef -> Eff (props :: ReactProps props, refs :: ReactRefs Disallowed, state :: ReactState ReadOnly state | eff) UI
```

A rendering function.

#### `UISpec`

``` purescript
type UISpec props state eff = { getInitialState :: UIRef -> Eff (props :: ReactProps props, state :: ReactState Disallowed state, refs :: ReactRefs Disallowed | eff) state, componentWillMount :: UIRef -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs Disallowed | eff) Unit, componentDidMount :: UIRef -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Unit, componentWillReceiveProps :: UIRef -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Unit, shouldComponentUpdate :: UIRef -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Boolean, componentWillUpdate :: UIRef -> Eff (props :: ReactProps props, state :: ReactState ReadWrite state, refs :: ReactRefs ReadOnly | eff) Unit, componentDidUpdate :: UIRef -> Eff (props :: ReactProps props, state :: ReactState ReadOnly state, refs :: ReactRefs ReadOnly | eff) Unit, componentWillUnmount :: UIRef -> Eff (props :: ReactProps props, state :: ReactState ReadOnly state, refs :: ReactRefs ReadOnly | eff) Unit }
```

A specification of a component.

#### `spec`

``` purescript
spec :: forall props state eff. state -> UISpec props state eff
```

Create a component specification.

#### `getProps`

``` purescript
getProps :: forall props eff. UIRef -> Eff (props :: ReactProps props | eff) props
```

Read the component props.

#### `getRefs`

``` purescript
getRefs :: forall write eff. UIRef -> Eff (refs :: ReactRefs (Read write) | eff) Refs
```

Read the component refs.

#### `writeState`

``` purescript
writeState :: forall state eff. UIRef -> state -> Eff (state :: ReactState ReadWrite state | eff) state
```

Write the component state.

#### `readState`

``` purescript
readState :: forall state write eff. UIRef -> Eff (state :: ReactState (Read write) state | eff) state
```

Read the component state.

#### `transformState`

``` purescript
transformState :: forall state statePerms eff. UIRef -> (state -> state) -> Eff (state :: ReactState ReadWrite state | eff) state
```

Transform the component state by applying a function.

#### `mkUI`

``` purescript
mkUI :: forall props state eff. UISpec props state eff -> Render props state eff -> props -> UI
```

Create a component from a component spec.

#### `handle`

``` purescript
handle :: forall eff ev props state result. (ev -> EventHandlerContext eff props state result) -> EventHandler ev
```

Create an event handler.

#### `renderToString`

``` purescript
renderToString :: UI -> String
```

Render a component as a string.

#### `renderToBody`

``` purescript
renderToBody :: forall eff. UI -> Eff (dom :: DOM | eff) UI
```

Render a component to the document body.

#### `renderToElementById`

``` purescript
renderToElementById :: forall eff. String -> UI -> Eff (dom :: DOM | eff) UI
```

Render a component to the element with the specified ID.


