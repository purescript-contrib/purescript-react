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
data Disallowed :: !
```

This phantom effect indicates that both read and write access to a resource are disallowed.

#### `ReadAllowed`

``` purescript
data ReadAllowed :: !
```

This phantom effect indicates that only read access to a resource is allowed.

#### `WriteAllowed`

``` purescript
data WriteAllowed :: !
```

This phantom effect indicates that only write access to a resource is allowed.

#### `ReactState`

``` purescript
data ReactState :: # ! -> * -> !
```

This effect indicates that a computation may read or write the component state.

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
type EventHandlerContext eff props refs state result = Eff (props :: ReactProps props, refs :: ReactRefs refs, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state | eff) result
```

A function which handles events.

#### `Render`

``` purescript
type Render props refs state eff = UIRef -> Eff (props :: ReactProps props, refs :: Disallowed, state :: ReactState (read :: ReadAllowed) state | eff) UI
```

A rendering function.

#### `UISpec`

``` purescript
type UISpec props refs state eff = { getInitialState :: UIRef -> Eff (props :: ReactProps props, state :: Disallowed, refs :: Disallowed | eff) state, componentWillMount :: UIRef -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: Disallowed | eff) Unit, componentDidMount :: UIRef -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff) Unit, componentWillReceiveProps :: UIRef -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff) Unit, shouldComponentUpdate :: UIRef -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff) Boolean, componentWillUpdate :: UIRef -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff) Unit, componentDidUpdate :: UIRef -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed) state, refs :: ReactRefs refs | eff) Unit, componentWillUnmount :: UIRef -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed) state, refs :: ReactRefs refs | eff) Unit }
```

A specification of a component.

#### `spec`

``` purescript
spec :: forall props refs state eff. state -> UISpec props refs state eff
```

Create a component specification.

#### `getProps`

``` purescript
getProps :: forall props eff. UIRef -> Eff (props :: ReactProps props | eff) props
```

Read the component props.

#### `getRefs`

``` purescript
getRefs :: forall refs eff. UIRef -> Eff (refs :: ReactRefs refs | eff) refs
```

Read the component refs.

#### `writeState`

``` purescript
writeState :: forall state eff. UIRef -> state -> Eff (state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state | eff) state
```

Write the component state.

#### `readState`

``` purescript
readState :: forall state stateEff eff. UIRef -> Eff (state :: ReactState (read :: ReadAllowed | stateEff) state | eff) state
```

Read the component state.

#### `transformState`

``` purescript
transformState :: forall state statePerms eff. UIRef -> (state -> state) -> Eff (state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state | eff) state
```

Transform the component state by applying a function.

#### `mkUI`

``` purescript
mkUI :: forall props refs state eff. UISpec props refs state eff -> Render props refs state eff -> props -> UI
```

Create a component from a component spec.

#### `handle`

``` purescript
handle :: forall eff ev props refs state result. (ev -> EventHandlerContext eff props refs state result) -> EventHandler ev
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


