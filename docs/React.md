## Module React

#### `UI`

``` purescript
data UI :: *
```

#### `UIRef`

``` purescript
data UIRef :: # ! -> *
```

#### `EventHandler`

``` purescript
data EventHandler :: * -> *
```

#### `Disallowed`

``` purescript
data Disallowed :: !
```

#### `ReadAllowed`

``` purescript
data ReadAllowed :: !
```

#### `WriteAllowed`

``` purescript
data WriteAllowed :: !
```

#### `ReactState`

``` purescript
data ReactState :: # ! -> * -> !
```

#### `ReactProps`

``` purescript
data ReactProps :: * -> !
```

#### `ReactRefs`

``` purescript
data ReactRefs :: * -> !
```

#### `noop0`

``` purescript
noop0 :: forall eff result. Eff eff result
```

#### `noop1`

``` purescript
noop1 :: forall a eff result. a -> Eff eff result
```

#### `noop2`

``` purescript
noop2 :: forall a b eff result. a -> b -> Eff eff result
```

#### `Render`

``` purescript
type Render props refs state eff = Eff (props :: ReactProps props, refs :: Disallowed, state :: ReactState (read :: ReadAllowed) state | eff) UI
```

#### `UISpec`

``` purescript
type UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8 = { getInitialState :: Eff (props :: ReactProps props, state :: Disallowed, refs :: Disallowed | eff1) state, componentWillMount :: Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: Disallowed | eff2) Unit, componentDidMount :: Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff3) Unit, componentWillReceiveProps :: props -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff4) Unit, shouldComponentUpdate :: props -> state -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff5) Boolean, componentWillUpdate :: props -> state -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state, refs :: ReactRefs refs | eff6) Unit, componentDidUpdate :: props -> state -> Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed) state, refs :: ReactRefs refs | eff7) Unit, componentWillUnmount :: Eff (props :: ReactProps props, state :: ReactState (read :: ReadAllowed) state, refs :: ReactRefs refs | eff8) Unit }
```

#### `spec`

``` purescript
spec :: forall props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8. UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8
```

#### `getProps`

``` purescript
getProps :: forall props eff. Eff (props :: ReactProps props | eff) props
```

#### `getRefs`

``` purescript
getRefs :: forall refs eff. Eff (refs :: ReactRefs refs | eff) refs
```

#### `writeState`

``` purescript
writeState :: forall state statePerms eff. state -> Eff (state :: ReactState (read :: ReadAllowed, write :: WriteAllowed | statePerms) state | eff) state
```

#### `readState`

``` purescript
readState :: forall state statePerms eff. Eff (state :: ReactState (read :: ReadAllowed | statePerms) state | eff) state
```

#### `getSelf`

``` purescript
getSelf :: forall eff. Eff eff (UIRef eff)
```

#### `runUI`

``` purescript
runUI :: forall refEff eff result. UIRef refEff -> Eff refEff result -> Eff eff result
```

#### `mkUI`

``` purescript
mkUI :: forall props refs state eff eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8. UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8 -> Render props refs state eff -> props -> UI
```

#### `DOMEvent`

``` purescript
type DOMEvent = forall attrs. {  | attrs }
```

#### `DOMEventTarget`

``` purescript
type DOMEventTarget = forall attrs. {  | attrs }
```

#### `Event`

``` purescript
type Event = { bubbles :: Boolean, cancelable :: Boolean, currentTarget :: DOMEventTarget, defaultPrevented :: Boolean, eventPhase :: Number, isTrusted :: Boolean, nativeEvent :: DOMEvent, preventDefault :: {  } -> {  }, stopPropagation :: {  } -> {  }, target :: DOMEventTarget, timeStamp :: Number, eventType :: String }
```

#### `MouseEvent`

``` purescript
type MouseEvent = { pageX :: Number, pageY :: Number }
```

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = { altKey :: Boolean, ctrlKey :: Boolean, charCode :: Number, key :: String, keyCode :: Number, locale :: String, location :: Number, metaKey :: Boolean, repeat :: Boolean, shiftKey :: Boolean, which :: Number }
```

#### `EventHandlerContext`

``` purescript
type EventHandlerContext eff props refs state result = forall statePerms. Eff (props :: ReactProps props, refs :: ReactRefs refs, state :: ReactState (read :: ReadAllowed, write :: WriteAllowed | statePerms) state | eff) result
```

#### `handle`

``` purescript
handle :: forall eff ev props refs state result. (ev -> EventHandlerContext eff props refs state result) -> EventHandler ev
```

#### `renderToString`

``` purescript
renderToString :: UI -> String
```

#### `renderToBody`

``` purescript
renderToBody :: forall eff. UI -> Eff (dom :: DOM | eff) UI
```

#### `renderToElementById`

``` purescript
renderToElementById :: forall eff. String -> UI -> Eff (dom :: DOM | eff) UI
```

#### `deferred`

``` purescript
deferred :: forall a eff. Eff eff a -> Eff eff a
```


