module React where

import Control.Monad.Eff

foreign import data DOM :: !
foreign import data ReadReactState :: * -> !
foreign import data WriteReactState :: * -> !
foreign import data UI :: *
foreign import data EventHandler :: * -> *

foreign import mkUI
  " function mkUI(render) {          \
  \   return React.createClass({     \
  \     render: function() {         \
  \       return render(this.props); \
  \     }                            \
  \   });                            \
  \ }"
  :: forall props.
  (props -> UI)
  -> (props -> UI)

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
  \           var ui = render(this.props)(); \
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
  -> (props -> Eff (r :: ReadReactState state) UI)
  -> (props -> UI)

foreign import writeState
  " function writeState(state) {                   \
  \   __current.replaceState({state: state});      \
  \   return function() { return state; }          \
  \ }"
  :: forall state eff.
  state
  -> Eff (r :: ReadReactState state, w :: WriteReactState state | eff) state

foreign import readState
  " function readState() {          \
  \   return __current.state.state; \
  \ }"
  :: forall state eff. Eff (r :: ReadReactState state | eff) state

type Event = { }
type MouseEvent = { pageX :: Number, pageY :: Number }

foreign import handle
  " function handle(f) {              \
  \   var component = __current;      \
  \   return function(e) {            \
  \     __current = component;        \
  \     try {                         \
  \       f(e);                       \
  \     } finally {                   \
  \       __current = null;           \
  \     }                             \
  \   }                               \
  \ }"
  :: forall state result event.
  (Eff (r :: ReadReactState state, w :: WriteReactState state) result)
  -> EventHandler event

foreign import handleEvent
  "var handleEvent = handle"
  :: forall state result.
  (Event -> Eff (r :: ReadReactState state, w :: WriteReactState state) result)
  -> EventHandler Event

foreign import handleMouseEvent
  "var handleMouseEvent = handle"
  :: forall state result.
  (MouseEvent -> Eff (r :: ReadReactState state, w :: WriteReactState state) result)
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
