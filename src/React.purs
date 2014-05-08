module React where

  import Control.Monad.Eff

  foreign import data DOM :: !
  foreign import data ReadPropsEff :: * -> !
  foreign import data ReadStateEff :: * -> !
  foreign import data WriteStateEff :: * -> !
  foreign import data UI :: *
  foreign import data EventHandler :: * -> *

  foreign import noop0
    "function noop0() {}"
    :: forall eff result. Eff ( eff ) result

  foreign import noop1
    "var noop1 = noop0"
    :: forall a eff result. a -> Eff ( eff ) result

  foreign import noop2
    "var noop2 = noop0"
    :: forall a b eff result. a -> b -> Eff ( eff ) result

  type Render props = Eff (p :: ReadPropsEff props) UI

  type ReadProps eff props result = Eff (
    p :: ReadPropsEff props
    | eff
    ) result

  type ShouldComponentUpdate props =
    props -> Eff ( p :: ReadPropsEff props ) Boolean

  type UISpec eff props =
    { componentWillMount :: ReadProps eff props {}
    , componentDidMount ::  ReadProps eff props {}
    , componentWillReceiveProps :: props -> ReadProps eff props {}
    , shouldComponentUpdate :: ShouldComponentUpdate props
    , componentWillUpdate :: props -> ReadProps eff props {}
    , componentDidUpdate :: props -> ReadProps eff props {}
    , componentWillUnmount :: ReadProps eff props {}
    }

  type ReadState eff props state result = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state
    | eff
    ) result

  type ReadWriteState eff props state result = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state,
    w :: WriteStateEff state
    | eff
    ) result

  type StatefulRender props state = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state
    ) UI

  type StatefulShouldComponentUpdate props state =
    props -> state -> Eff (
      p :: ReadPropsEff props,
      r :: ReadStateEff state,
      w :: WriteStateEff state
      ) Boolean

  type StatefulUISpec eff props state =
    { componentWillMount :: ReadState eff props state {}
    , componentDidMount ::  ReadWriteState eff props state {}
    , componentWillReceiveProps :: props -> ReadWriteState eff props state {}
    , shouldComponentUpdate :: StatefulShouldComponentUpdate props state
    , componentWillUpdate :: props -> state -> ReadWriteState eff props state {}
    , componentDidUpdate :: props -> state -> ReadState eff props state {}
    , componentWillUnmount :: ReadState eff props state {}
    }

  defaultSpec =
    { componentWillMount: noop0
    , componentDidMount: noop0
    , componentWillReceiveProps: noop1
    , shouldComponentUpdate: updateAlways
    , componentWillUpdate: noop2
    , componentDidUpdate: noop2
    , componentWillUnmount: noop0
    }
      where
    updateAlways :: forall props. ShouldComponentUpdate props
    updateAlways props = return true

  defaultStatefulSpec =
    { componentWillMount: noop0
    , componentDidMount: noop0
    , componentWillReceiveProps: noop1
    , shouldComponentUpdate: updateAlways
    , componentWillUpdate: noop2
    , componentDidUpdate: noop2
    , componentWillUnmount: noop0
    }
      where
    updateAlways :: forall props state. StatefulShouldComponentUpdate props state
    updateAlways props state = return true

  foreign import mkUI
    " function mkUI(render) {            \
    \   return React.createClass({       \
    \     render: function() {           \
    \      __current = this;             \
    \      try {                         \
    \        var ui = render.call(this); \
    \      } finally {                   \
    \        __current = null;           \
    \      }                             \
    \      return ui;                    \
    \     }                              \
    \   });                              \
    \ }"
    :: forall props.
    Render props
    -> (props -> UI)

  foreign import mkUIFromSpec
    " function mkUIFromSpec(render) {                   \
    \   return function(ss) {                           \
    \     var specs = {};                               \
    \     for (var s in ss) {                           \
    \       if (ps.hasOwnProperty(s)) {                 \
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
    \     specs.render = function() {                   \
    \       __current = this;                           \
    \       try {                                       \
    \         var ui = render();                        \
    \       } finally {                                 \
    \         __current = null;                         \
    \       }                                           \
    \       return ui;                                  \
    \     };                                            \
    \     return React.createClass(specs);              \
    \   };                                              \
    \ }"
    :: forall eff props.
    Render props
    -> UISpec eff props
    -> (props -> UI)

  foreign import getProps
    " function getProps() {     \
    \   return __current.props; \
    \ }"
    :: forall props eff.
    Eff (p :: ReadPropsEff props | eff) props

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
    \           var ui = render.call(this);    \
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
    " var __current;                                      \
    \ function mkStatefulUIFromSpec(state) {              \
    \   return function(render) {                         \
    \     return function (ss) {                          \
    \       var specs = {};                               \
    \       for (var s in ss) {                           \
    \         if (ss.hasOwnProperty(s)) {                 \
    \           specs[s] = (function(impl) {              \
    \             return function() {                     \
    \               __current = this;                     \
    \               try {                                 \
    \                 return impl.apply(this, arguments); \
    \               } finally {                           \
    \                 __current = null;                   \
    \               }                                     \
    \             }                                       \
    \           })(ss[s]);                                \
    \         }                                           \
    \       }                                             \
    \       specs.getInitialState = function() {          \
    \         return {state: state};                      \
    \       };                                            \
    \       specs.render = function() {                   \
    \         __current = this;                           \
    \         try {                                       \
    \           var ui = render.call(this);               \
    \         } finally {                                 \
    \           __current = null;                         \
    \         }                                           \
    \         return ui;                                  \
    \       };                                            \
    \       return React.createClass(specs);              \
    \     }                                               \
    \   }                                                 \
    \ }"
    :: forall eff props state.
    state
    -> StatefulRender props state
    -> StatefulUISpec eff props state
    -> (props -> UI)

  foreign import writeState
    " function writeState(state) {                   \
    \   __current.replaceState({state: state});      \
    \   return function() { return state; }          \
    \ }"
    :: forall state eff.
    state
    -> Eff (r :: ReadStateEff state, w :: WriteStateEff state | eff) state

  foreign import readState
    " function readState() {    \
    \   return __current.state.state; \
    \ }"
    :: forall state eff. Eff (r :: ReadStateEff state | eff) state

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
