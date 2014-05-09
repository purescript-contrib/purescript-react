module React where

import Control.Monad.Eff

foreign import data DOM :: !
foreign import data ReactProps :: * -> !
foreign import data ReactRefs :: * -> !
foreign import data ReadReactState :: * -> !
foreign import data WriteReactState :: * -> !
foreign import data UI :: *
foreign import data EventHandler :: * -> *

type Render props = Eff (p :: ReactProps props) UI
type UISpec props = forall r.
  { render :: Render props }

type StatefulRender props state = Eff (p :: ReactProps props, r :: ReadReactState state) UI
type StatefulUISpec props state = forall a r.
  { render :: StatefulRender props state
  , getInitialState :: {} -> state
  , componentWillMount :: {} -> {}
  , componentDidMount :: {} -> {}
  , componentWillReceiveProps :: props -> {}
  , shouldComponentUpdate :: props -> state -> Boolean
  , componentWillUpdate :: props -> state -> {}
  , componentDidUpdate :: props -> state -> {}
  , componentWillUnmount :: {} -> {}
  }

defaultStatefulSpec =
  { render: defaultRender
  , getInitialState: \_ -> {}
  , componentWillMount: \_ -> {}
  , componentDidMount: \_ -> {}
  , componentWillReceiveProps: \_ -> {}
  , shouldComponentUpdate: \_ _ -> true
  , componentWillUpdate: \_ _ -> {}
  , componentDidUpdate: \_ _ -> {}
  , componentWillUnmount: \_ -> {}
  }

foreign import defaultRender
  " function defaultRender() { \
  \   return React.DOM.p({});  \
  \ }"
  :: forall props state. StatefulRender props state

foreign import mkUI
  " function mkUI(render) {          \
  \   return React.createClass({     \
  \     render: function() {         \
  \      __current = this;           \
  \      try {                       \
  \        var ui = render(); \
  \      } finally {                 \
  \        __current = null;         \
  \      }                           \
  \      return ui;                  \
  \     }                            \
  \   });                            \
  \ }"
  :: forall props.
  Render props
  -> (props -> UI)

foreign import mkUIFromSpec
  " function mkUIFromSpec(ps) {         \
  \   var props = {};                   \
  \   for (var p in ps) {               \
  \     if (ps.hasOwnProperty(p)) {     \
  \       props[p] = ps[p];             \
  \     }                               \
  \   }                                 \
  \   props.render = function() {       \
  \     __current = this;               \
  \     try {                           \
  \       var ui = ps.render();         \
  \     } finally {                     \
  \       __current = null;             \
  \     }                               \
  \     return ui;                      \
  \   };                                \
  \   return React.createClass(props);  \
  \ }"
  :: forall props.
  UISpec props
  -> (props -> UI)

foreign import getProps
  " function getProps() {     \
  \   return __current.props; \
  \ }"
  :: forall props eff.
  Eff (p :: ReactProps props | eff) props

foreign import getRefs
  " function getRefs() {     \
  \   return __current.refs; \
  \ }"
  :: forall refs eff.
  Eff (f :: ReactRefs refs | eff) refs

foreign import mkStatefulUI
  " var __current;                           \
  \ function mkStatefulUI(state) {           \
  \   return function(render) {              \
  \     return React.createClass({           \
  \                                          \
  \       getInitialState: function() {      \
  \         return {state: state};           \
  \       },                                 \
  \                                          \
  \       render: function() {               \
  \         __current = this;                \
  \         try {                            \
  \           var ui = render();             \
  \         } finally {                      \
  \           __current = null;              \
  \         }                                \
  \         return ui;                       \
  \       }                                  \
  \     });                                  \
  \   };                                     \
  \ }"
  :: forall props state.
  state
  -> StatefulRender props state
  -> (props -> UI)

foreign import mkStatefulUIFromSpec
  " var __current;                      \
  \ function mkStatefulUIFromSpec(ss) { \
  \   var specs = {};                   \
  \   for (var s in ss) {               \
  \     if (ss.hasOwnProperty(s)) {     \
  \       specs[s] = ss[s];             \
  \     }                               \
  \   }                                 \
  \   specs.render = function() {       \
  \     __current = this;               \
  \     try {                           \
  \       var ui = ss.render();         \
  \     } finally {                     \
  \       __current = null;             \
  \     }                               \
  \     return ui;                      \
  \   };                                \
  \   return React.createClass(specs);  \
  \ }"
  :: forall props state.
  StatefulUISpec props state
  -> (props -> UI)

foreign import writeState
  " function writeState(state) {                   \
  \   __current.replaceState(state);      \
  \   return function() { return state; }          \
  \ }"
  :: forall state eff.
  state
  -> Eff (r :: ReadReactState state, w :: WriteReactState state | eff) state

foreign import readState
  " function readState() {    \
  \   return __current.state; \
  \ }"
  :: forall state eff. Eff (r :: ReadReactState state | eff) state

type Event = { }
type MouseEvent = { pageX :: Number, pageY :: Number }

type EventHandlerContext eff props state result = Eff (
  p :: ReactProps props,
  r :: ReadReactState state,
  w :: WriteReactState state
  | eff
  ) result

foreign import handle
  " function handle(f) {                  \
  \   var component = __current;          \
  \   return function(e) {                \
  \     __current = component;            \
  \     try {                             \
  \       var res = f.call(__current, e); \
  \     } finally {                       \
  \       __current = null;               \
  \     }                                 \
  \     return res;                       \
  \   }                                   \
  \ }"
  :: forall eff props state result event.
  EventHandlerContext eff props state result -> EventHandler event

foreign import handleEvent
  "var handleEvent = handle"
  :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result)
  -> EventHandler Event

foreign import handleMouseEvent
  "var handleMouseEvent = handle"
  :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result)
  -> EventHandler MouseEvent

foreign import renderToString
  "var renderToString = React.renderComponentToString"
  :: UI -> String

foreign import renderToBody
  " function renderToBody(component) {                          \
  \   return function() {                                       \
  \     return React.renderComponent(component, document.body); \
  \   }                                                         \
  \ }"
  :: forall eff. UI -> Eff (dom :: DOM | eff) UI

foreign import renderToElementById
  " function renderToElementById(id) {                                          \
  \   return function(component) {                                              \
  \     return function() {                                                     \
  \       return React.renderComponent(component, document.getElementById(id)); \
  \     }                                                                       \
  \   }                                                                         \
  \ }"
  :: forall eff. String -> UI -> Eff (dom :: DOM | eff) UI
