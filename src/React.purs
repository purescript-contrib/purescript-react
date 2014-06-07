module React where

  import Debug.Trace
  import Control.Monad.Eff

  foreign import data DOM :: !
  foreign import data ReadPropsEff :: * -> !
  foreign import data ReadRefsEff :: * -> !
  foreign import data ReadStateEff :: * -> !
  foreign import data WriteStateEff :: * -> !
  foreign import data UI :: *
  foreign import data UIRef :: * -> * -> *
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

  type ReadProps props refs result = Eff (
    p :: ReadPropsEff props,
    f :: ReadRefsEff refs,
    dom :: DOM,
    trace :: Trace
    ) result

  type ReadState props refs state result = Eff (
    p :: ReadPropsEff props,
    f :: ReadRefsEff refs,
    r :: ReadStateEff state,
    dom :: DOM,
    trace :: Trace
    ) result

  type ReadWriteState props refs state result = Eff (
    p :: ReadPropsEff props,
    f :: ReadRefsEff refs,
    r :: ReadStateEff state,
    w :: WriteStateEff state,
    dom :: DOM,
    trace :: Trace
    ) result

  type Render props refs state = Eff (
    p :: ReadPropsEff props,
    f :: ReadRefsEff refs,
    r :: ReadStateEff state,
    trace :: Trace
    ) UI

  type ShouldComponentUpdate props refs state =
    props -> state -> Eff (
      p :: ReadPropsEff props,
      f :: ReadRefsEff refs,
      r :: ReadStateEff state,
      w :: WriteStateEff state,
      trace :: Trace
      ) Boolean

  type UISpec props refs state =
    { getInitialState :: ReadProps props refs state
    , componentWillMount :: ReadState props refs state {}
    , componentDidMount ::  ReadWriteState props refs state {}
    , componentWillReceiveProps :: props -> ReadWriteState props refs state {}
    , shouldComponentUpdate :: ShouldComponentUpdate refs props state
    , componentWillUpdate :: props -> state -> ReadWriteState props refs state {}
    , componentDidUpdate :: props -> state -> ReadState props refs state {}
    , componentWillUnmount :: ReadState props refs state {}
    }

  spec :: forall props refs state. UISpec props refs state
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
    updateAlways :: forall props refs state. ShouldComponentUpdate props refs state
    updateAlways props state = return true

  foreign import getProps
    " function getProps() {     \
    \   return __current.props; \
    \ }"
    :: forall props eff.
    Eff (p :: ReadPropsEff props | eff) props

  foreign import getRefs
    " function getRefs() {     \
    \   return __current.refs; \
    \ }"
    :: forall refs eff.
    Eff (f :: ReadRefsEff refs | eff) refs

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

  transformState f = do
    state <- readState
    writeState $ f state

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
    :: forall props refs state.
    UISpec props refs state
    -> Render props refs state
    -> (props -> UI)

  type DOMEvent = forall attrs. { | attrs}
  type DOMEventTarget = forall attrs. { | attrs }
  type Event = { bubbles           :: Boolean
               , cancelable        :: Boolean
               , currentTarget     :: DOMEventTarget
               , defaultPrevented  :: Boolean
               , eventPhase        :: Number
               , isTrusted         :: Boolean
               , nativeEvent       :: DOMEvent
               , preventDefault    :: {} -> {}
               , stopPropagation   :: {} -> {}
               , target            :: DOMEventTarget
               , timeStamp         :: Number
               , eventType         :: String
               }
  type MouseEvent = { pageX :: Number, pageY :: Number }
  type KeyboardEvent = { altKey   :: Boolean
                       , ctrlKey  :: Boolean
                       , charCode :: Number
                       , key      :: String
                       , keyCode  :: Number
                       , locale   :: String
                       , location :: Number
                       , metaKey  :: Boolean
                       , repeat   :: Boolean
                       , shiftKey :: Boolean
                       , which    :: Number
                       }

  type EventHandlerContext eff props state result = Eff (
    p :: ReadPropsEff props,
    r :: ReadStateEff state,
    w :: WriteStateEff state
    | eff
    ) result

  foreign import handle
    " function handle(f) {                    \
    \   var component = __current;            \
    \   return function(e) {                  \
    \     __current = component;              \
    \     try {                               \
    \       var res = f.call(__current, e)(); \
    \     } finally {                         \
    \       __current = null;                 \
    \     }                                   \
    \     return res;                         \
    \   }                                     \
    \ }"
    :: forall eff ev props state result.
    (ev -> EventHandlerContext eff props state result)
    -> EventHandler ev

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

  foreign import deferred
    "function deferred(action) {\
    \  return function() {\
    \  var component = __current;\
    \  return function() {\
    \    if (!component.isMounted()) {\
    \      return;\
    \    }\
    \    __current = component;\
    \    try {\
    \      return action();\
    \    } finally {\
    \      __current = null;\
    \    }\
    \  };\
    \  };\
    \}" :: forall props state result eff eff2.
    Eff (p :: ReadPropsEff props, r :: ReadStateEff state, w :: WriteStateEff state | eff) result
    -> Eff (eff) (Eff (eff2) result)

  foreign import deferred1
    "function deferred(action) {\
    \  return function() {\
    \  var component = __current;\
    \  return function(a) {\
    \    if (!component.isMounted()) {\
    \      return;\
    \    }\
    \    __current = component;\
    \    try {\
    \      return action(a);\
    \    } finally {\
    \      __current = null;\
    \    }\
    \  };\
    \  };\
    \}" :: forall a props state result eff eff2.
    (a -> Eff (p :: ReadPropsEff props, r :: ReadStateEff state, w :: WriteStateEff state | eff) result)
    -> Eff (eff) (a -> Eff (eff2) result)
