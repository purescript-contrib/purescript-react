module React where

  import Control.Monad.Eff

  foreign import data DOM :: !
  foreign import data ReadPropsEff :: * -> !
  foreign import data ReadStateEff :: * -> !
  foreign import data WriteStateEff :: * -> !
  foreign import data UI :: *
  foreign import data EventHandler :: * -> *

  foreign import noop0
    "function noop0() { return null; }"
    :: forall eff result. Eff ( eff ) result

  foreign import noop1
    "var noop1 = noop0"
    :: forall a eff result. a -> Eff ( eff ) result

  foreign import noop2
    "var noop2 = noop0"
    :: forall a b eff result. a -> b -> Eff ( eff ) result

  type ReadProps eff props result = Eff (
    p :: ReadPropsEff props,
    dom :: DOM
    ) result

  type ReadState eff props state result = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state,
    dom :: DOM
    ) result

  type ReadWriteState eff props state result = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state,
    w :: WriteStateEff state,
    dom :: DOM
    ) result

  type Render props state = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state
    ) UI

  type ShouldComponentUpdate props state =
    props -> state -> Eff (
      p :: ReadPropsEff props,
      r :: ReadStateEff state,
      w :: WriteStateEff state
      ) Boolean

  type UISpec eff props state =
    { getInitialState :: ReadProps eff props state
    , componentWillMount :: ReadState eff props state {}
    , componentDidMount ::  ReadWriteState eff props state {}
    , componentWillReceiveProps :: props -> ReadWriteState eff props state {}
    , shouldComponentUpdate :: ShouldComponentUpdate props state
    , componentWillUpdate :: props -> state -> ReadWriteState eff props state {}
    , componentDidUpdate :: props -> state -> ReadState eff props state {}
    , componentWillUnmount :: ReadState eff props state {}
    }

  spec :: forall eff props state. UISpec eff props state
  spec =
    { getInitialState: noop0
    , componentWillMount: noop0
    , componentDidMount: noop0
    , componentWillReceiveProps: noop1
    , shouldComponentUpdate: updateAlways
    , componentWillUpdate: noop2
    , componentDidUpdate: noop2
    , componentWillUnmount: noop0
    }
      where
    updateAlways :: forall props state. ShouldComponentUpdate props state
    updateAlways props state = return true

  foreign import getProps
    " function getProps() {     \
    \   return __current.props; \
    \ }"
    :: forall props eff.
    Eff (p :: ReadPropsEff props | eff) props

  foreign import writeState
    " function writeState(state) {                   \
    \   __current.replaceState({state: state});      \
    \   return function() { return state; }          \
    \ }"
    :: forall state eff.
    state
    -> Eff (r :: ReadStateEff state, w :: WriteStateEff state | eff) state

  foreign import readState
    " function readState() {          \
    \   return __current.state.state; \
    \ }"
    :: forall state eff. Eff (r :: ReadStateEff state | eff) state

  foreign import mkUI
    " var __current;                                    \
    \ function mkUI(ss) {                               \
    \   return function(render) {                       \
    \     var specs = {};                               \
    \     for (var s in ss) {                           \
    \       if (ss.hasOwnProperty(s)) {                 \
    \         specs[s] = (function(impl) {              \
    \           return function() {                     \
    \             __current = this;                     \
    \             try {                                 \
    \               return impl.apply(this, arguments); \
    \             } finally {                           \
    \               __current = null;                   \
    \             }                                     \
    \           }                                       \
    \         })(ss[s]);                                \
    \       }                                           \
    \     }                                             \
    \     specs.getInitialState= function() {           \
    \       __current = this;                           \
    \       try {                                       \
    \         return {state: ss.getInitialState()};     \
    \       } finally {                                 \
    \         __current = null;                         \
    \       }                                           \
    \     };                                            \
    \     specs.render = function() {                   \
    \       __current = this;                           \
    \       try {                                       \
    \         var ui = render.call(this);               \
    \       } finally {                                 \
    \         __current = null;                         \
    \       }                                           \
    \       return ui;                                  \
    \     };                                            \
    \     return React.createClass(specs);              \
    \   }                                               \
    \ }"
    :: forall eff props state.
    UISpec eff props state
    -> Render props state
    -> (props -> UI)

  type Event = { }
  type MouseEvent = { pageX :: Number, pageY :: Number }

  type EventHandlerContext eff props state result = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state,
    w :: WriteStateEff state
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
    EventHandlerContext props state result eff -> EventHandler event

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
