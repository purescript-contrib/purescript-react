module React where

  import Control.Monad.Eff (Eff())

  import Data.Foreign.EasyFFI
  import Data.Function (mkFn1, Fn0(), Fn1(), Fn2())

  import DOM (DOM())

  foreign import data Component :: *
  foreign import data Element :: *
  foreign import data Event :: *
  foreign import data React :: !
  foreign import document ::
    { getElementById :: String -> Element
    , body :: Element
    }

  -- A ComponentClass is a builder for a Component.
  type ComponentClass props state = props -> [Component] -> Component
  -- A Component is an instantiated Component, i.e. it has props and children.
  type This fields = { | fields}

  type Spec fields props state s =
    { render :: Render fields props { | state}
    , getInitialState :: forall eff. This fields -> Eff (react :: React, dom :: DOM | eff) { | state}
    , componentDidMount :: forall eff. ReactThis fields props { | state} -> Eff (react :: React, dom :: DOM | eff) Unit
    | s
    }

  type ReactThis fields props state = This
    ( state :: state
    , props :: props
    , replaceState :: state -> Unit
    | fields
    )

  type Render fields props state = forall eff
    .  ReactThis fields props state
    -> Eff (react :: React, dom :: DOM | eff) Component

  foreign import spec
    "var spec = {};" :: forall fields state props s. Spec fields props state s

  foreign import createClass
    "function createClass(psSpec) {\
    \  var spec = {};\
    \  for (var fun in psSpec) {\
    \    if (psSpec.hasOwnProperty(fun)) {\
    \      (function(f) {\
    \        if (typeof psSpec[f] === 'function') {\
    \          spec[f] = function() {\
    \            return psSpec[f].apply(this, [this].concat([].slice.call(arguments)))() ;\
    \          }\
    \        } else {\
    \          spec[f] = f;\
    \        }\
    \      })(fun);\
    \    }\
    \  }\
    \  return function(props) {\
    \    return function(children) {\
    \      return React.createClass(spec)(props, children);\
    \    }\
    \  }\
    \}" :: forall s fields state props
        .  Spec fields props state s
        -> ComponentClass props { | state}

  foreign import renderComponent
    "function renderComponent(component) {\
    \  return function(element) {\
    \    return function() {\
    \      return React.renderComponent(component, element);\
    \    }\
    \  }\
    \}" :: forall eff. Component -> Element -> Eff (react :: React, dom :: DOM | eff) Component

  renderToId :: forall eff
             .  String
             -> Component
             -> Eff (react :: React, dom :: DOM | eff) Component
  renderToId selector component =
    renderComponent component (document.getElementById selector)

  foreign import eventHandler
    "function eventHandler(f) {\
    \  return function(e) {\
    \    return f(e)();\
    \  }\
    \}" :: forall eff
       .  (Event -> Eff (react :: React, dom :: DOM | eff) Unit)
       -> Event
       -> Unit
