# Module Documentation

## Module React

### Values

    createClass :: forall s fields state props eff mixins statics. Spec fields props state s eff mixins statics -> ComponentClass {  | props } {  | state }

    document :: { body :: Element, getElementById :: String -> Element }

    eventHandler :: forall eff fields fields' event a. This fields -> (This fields' -> ReactSyntheticEvent event -> Eff (dom :: DOM, react :: React | eff) a) -> ReactSyntheticEvent event -> Unit

    renderComponent :: forall eff. Component -> Element -> Eff (dom :: DOM, react :: React | eff) Component

    renderComponentById :: forall eff. Component -> String -> Eff (dom :: DOM, react :: React | eff) Component

    spec :: forall fields state props s eff mixins statics. Spec fields props state s eff mixins statics


## Module React.DOM

### Values

    dom :: forall props. String -> props -> [Component] -> Component

    rawText :: String -> Component


## Module React.Types

### Types

    data BlackList :: *

    data Component :: *

    type ComponentClass props state = props -> [Component] -> Component

    type ComponentDidMount fields props state eff = ReactThis (getDOMNode :: Eff (dom :: DOM, react :: React | eff) Element | fields) props state -> Eff (dom :: DOM, react :: React | eff) Unit

    type ComponentDidUpdate fields props state eff = Fn3 (ReactThis fields props state) props state (Eff (dom :: DOM, react :: React | eff) Boolean)

    type ComponentWillMount fields props state eff = ReactThis (getDOMNode :: BlackList | fields) props state -> Eff (dom :: DOM, react :: React | eff) Unit

    type ComponentWillReceiveProps fields props state eff = Fn2 (ReactThis fields props state) props (Eff (dom :: DOM, react :: React | eff) Boolean)

    type ComponentWillUnmount fields props state eff = ReactThis (getDOMNode :: Eff (dom :: DOM, react :: React | eff) Element | fields) props state -> Eff (dom :: DOM, react :: React | eff) Unit

    type ComponentWillUpdate fields props state eff = Fn3 (This (setState :: BlackList, replaceState :: BlackList, getDOMNode :: Eff (dom :: DOM, react :: React | eff) Element, forceUpdate :: Eff (dom :: DOM, react :: React | eff) Unit, state :: props, props :: props | fields)) props state (Eff (dom :: DOM, react :: React | eff) Boolean)

    data DOMEvent :: *

    data DOMEventTarget :: *

    data Element :: *

    data Event :: !

    type GetInitialState fields state eff = This (setState :: BlackList, replaceState :: BlackList, getDOMNode :: BlackList, forceUpdate :: BlackList, state :: BlackList, props :: BlackList | fields) -> Eff (dom :: DOM, react :: React | eff) state

    data React :: !

    type ReactFormEvent  = ReactSyntheticEvent (unit :: Unit)

    type ReactSyntheticEvent fields = { "type" :: String, timeStamp :: Number, target :: DOMEventTarget, stopPropagation :: forall eff. Eff (event :: Event | eff) Unit, preventDefault :: forall eff. Eff (event :: Event | eff) Unit, nativeEvent :: DOMEvent, isTrusted :: Boolean, eventPhase :: Number, defaultPrevented :: Boolean, currentTarget :: DOMEventTarget, cancelable :: Boolean, bubbles :: Boolean | fields }

    type ReactThis fields props state = This (replaceState :: state -> Unit, setState :: state -> Unit, refs :: forall r. {  | r }, props :: props, state :: state | fields)

    type Render fields props state eff = This (setState :: BlackList, replaceState :: BlackList, getDOMNode :: BlackList, forceUpdate :: BlackList, state :: state, props :: props | fields) -> Eff (dom :: DOM, react :: React | eff) Component

    type ShouldComponentUpdate fields props state eff = Fn3 (ReactThis fields props state) props state (Eff (dom :: DOM, react :: React | eff) Boolean)

    type Spec fields props state s eff mixins statics = { statics :: {  | statics }, mixins :: [{  | mixins }], displayName :: String, shouldComponentUpdate :: ShouldComponentUpdate fields {  | props } {  | state } eff, getInitialState :: GetInitialState fields {  | state } eff, componentWillUpdate :: ComponentWillUpdate fields {  | props } {  | state } eff, componentWillUnmount :: ComponentWillUnmount fields {  | props } {  | state } eff, componentWillReceiveProps :: ComponentWillReceiveProps fields {  | props } {  | state } eff, componentWillMount :: ComponentWillMount fields {  | props } {  | state } eff, componentDidUpdate :: ComponentDidUpdate fields {  | props } {  | state } eff, componentDidMount :: ComponentDidMount fields {  | props } {  | state } eff, render :: Render fields { children :: [Component] | props } {  | state } eff | s }

    type This fields = { isMounted :: forall eff. Eff (react :: React | eff) Boolean | fields }



