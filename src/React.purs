module React where

  import Control.Monad.Eff (Eff())

  import DOM (DOM())

  import React.Types
    ( Component()
    , ComponentClass()
    , Element()
    , React()
    , ReactSyntheticEvent()
    , Spec()
    , This()
    )

  foreign import document ::
    { getElementById :: String -> Element
    , body :: Element
    }

  foreign import spec
    "var spec = {};" :: forall fields state props s eff mixins statics
                     .  Spec   fields props state s eff mixins statics

  foreign import coerceThis
    "function coerceThis(that) {\
    \  return that;\
    \}" :: forall fields fields'. This fields -> This fields'

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
    \          spec[f] = psSpec[f];\
    \        }\
    \      })(fun);\
    \    }\
    \  }\
    \  var Class = React.createClass(spec);\
    \  return function(props) {\
    \    return function(children) {\
    \      return Class(props, children);\
    \    }\
    \  }\
    \}" :: forall s fields state props eff mixins statics
        .  Spec fields props state s eff mixins statics
        -> ComponentClass { | props} { | state}

  foreign import renderComponent
    "function renderComponent(component) {\
    \  return function(element) {\
    \    return function() {\
    \      return React.renderComponent(component, element);\
    \    }\
    \  }\
    \}" :: forall eff. Component -> Element -> Eff (react :: React, dom :: DOM | eff) Component

  foreign import eventHandler
    "function eventHandler(that) {\
    \  return function(f) {\
    \    return function(e) {\
    \      return f(that)(e)();\
    \    }\
    \  }\
    \}" :: forall eff fields event a
       .  This fields
       -> (This fields -> ReactSyntheticEvent event -> Eff (react :: React, dom :: DOM | eff) a)
       -> ReactSyntheticEvent event
       -> Unit

  renderComponentById :: forall eff
             .  Component
             -> String
             -> Eff (react :: React, dom :: DOM | eff) Component
  renderComponentById component id =
    renderComponent component $ document.getElementById id
