module React (EventHandlerContext(), KeyboardEvent(), MouseEvent(), Event(), DOMEventTarget(), DOMEvent(), UISpec(), Render(), ReactRefs(), ReactProps(), ReactState(), WriteAllowed(), ReadAllowed(), Disallowed(), EventHandler(), UIRef(), UI(), DOM(), deferred, renderToElementById, renderToBody, renderToString, handle, mkUI, runUI, getSelf, transformState, readState, writeState, getRefs, getProps, spec, noop2, noop1, noop0) where
import Prelude ()
import React ()
import Prim ()
import Prelude ()
import Control.Monad.Eff ()
import Control.Monad.Eff.Console ()
type EventHandlerContext (eff :: # !) (props :: *) (refs :: *) (state :: *) (result :: *) = forall statePerms. Control.Monad.Eff.Eff (state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed | statePerms) state, refs :: React.ReactRefs refs, props :: React.ReactProps props | eff) result
type KeyboardEvent = { which :: Prim.Number, shiftKey :: Prim.Boolean, repeat :: Prim.Boolean, metaKey :: Prim.Boolean, location :: Prim.Number, locale :: Prim.String, keyCode :: Prim.Number, key :: Prim.String, charCode :: Prim.Number, ctrlKey :: Prim.Boolean, altKey :: Prim.Boolean }
type MouseEvent = { pageY :: Prim.Number, pageX :: Prim.Number }
type Event = { eventType :: Prim.String, timeStamp :: Prim.Number, target :: React.DOMEventTarget, stopPropagation :: {  } -> {  }, preventDefault :: {  } -> {  }, nativeEvent :: React.DOMEvent, isTrusted :: Prim.Boolean, eventPhase :: Prim.Number, defaultPrevented :: Prim.Boolean, currentTarget :: React.DOMEventTarget, cancelable :: Prim.Boolean, bubbles :: Prim.Boolean }
type DOMEventTarget = forall attrs. {  | attrs }
type DOMEvent = forall attrs. {  | attrs }
type UISpec (props :: *) (refs :: *) (state :: *) (eff1 :: # !) (eff2 :: # !) (eff3 :: # !) (eff4 :: # !) (eff5 :: # !) (eff6 :: # !) (eff7 :: # !) (eff8 :: # !) = { componentWillUnmount :: Control.Monad.Eff.Eff (refs :: React.ReactRefs refs, state :: React.ReactState (read :: React.ReadAllowed) state, props :: React.ReactProps props | eff8) Prelude.Unit, componentDidUpdate :: props -> state -> Control.Monad.Eff.Eff (refs :: React.ReactRefs refs, state :: React.ReactState (read :: React.ReadAllowed) state, props :: React.ReactProps props | eff7) Prelude.Unit, componentWillUpdate :: props -> state -> Control.Monad.Eff.Eff (refs :: React.ReactRefs refs, state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed) state, props :: React.ReactProps props | eff6) Prelude.Unit, shouldComponentUpdate :: props -> state -> Control.Monad.Eff.Eff (refs :: React.ReactRefs refs, state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed) state, props :: React.ReactProps props | eff5) Prim.Boolean, componentWillReceiveProps :: props -> Control.Monad.Eff.Eff (refs :: React.ReactRefs refs, state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed) state, props :: React.ReactProps props | eff4) Prelude.Unit, componentDidMount :: Control.Monad.Eff.Eff (refs :: React.ReactRefs refs, state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed) state, props :: React.ReactProps props | eff3) Prelude.Unit, componentWillMount :: Control.Monad.Eff.Eff (refs :: React.Disallowed, state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed) state, props :: React.ReactProps props | eff2) Prelude.Unit, getInitialState :: Control.Monad.Eff.Eff (refs :: React.Disallowed, state :: React.Disallowed, props :: React.ReactProps props | eff1) state }
type Render (props :: *) (refs :: *) (state :: *) (eff :: # !) = Control.Monad.Eff.Eff (state :: React.ReactState (read :: React.ReadAllowed) state, refs :: React.Disallowed, props :: React.ReactProps props | eff) React.UI
foreign import data ReactRefs :: * -> !
foreign import data ReactProps :: * -> !
foreign import data ReactState :: # ! -> * -> !
foreign import data WriteAllowed :: !
foreign import data ReadAllowed :: !
foreign import data Disallowed :: !
foreign import data EventHandler :: * -> *
foreign import data UIRef :: # ! -> *
foreign import data UI :: *
foreign import data DOM :: !
foreign import deferred :: forall a eff. Control.Monad.Eff.Eff eff a -> Control.Monad.Eff.Eff eff a
foreign import renderToElementById :: forall eff. Prim.String -> React.UI -> Control.Monad.Eff.Eff (dom :: React.DOM | eff) React.UI
foreign import renderToBody :: forall eff. React.UI -> Control.Monad.Eff.Eff (dom :: React.DOM | eff) React.UI
foreign import renderToString :: React.UI -> Prim.String
foreign import handle :: forall eff ev props refs state result. (ev -> React.EventHandlerContext eff props refs state result) -> React.EventHandler ev
foreign import mkUI :: forall props refs state eff eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8. React.UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8 -> React.Render props refs state eff -> props -> React.UI
foreign import runUI :: forall refEff eff result. React.UIRef refEff -> Control.Monad.Eff.Eff refEff result -> Control.Monad.Eff.Eff eff result
foreign import getSelf :: forall eff. Control.Monad.Eff.Eff eff (React.UIRef eff)
foreign import transformState :: forall t267 t272 t283. (t267 -> t267) -> Control.Monad.Eff.Eff (state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed | t283) t267 | t272) t267
foreign import readState :: forall state statePerms eff. Control.Monad.Eff.Eff (state :: React.ReactState (read :: React.ReadAllowed | statePerms) state | eff) state
foreign import writeState :: forall state statePerms eff. state -> Control.Monad.Eff.Eff (state :: React.ReactState (write :: React.WriteAllowed, read :: React.ReadAllowed | statePerms) state | eff) state
foreign import getRefs :: forall refs eff. Control.Monad.Eff.Eff (refs :: React.ReactRefs refs | eff) refs
foreign import getProps :: forall props eff. Control.Monad.Eff.Eff (props :: React.ReactProps props | eff) props
foreign import spec :: forall props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8. React.UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8
foreign import noop2 :: forall a b eff result. a -> b -> Control.Monad.Eff.Eff eff result
foreign import noop1 :: forall a eff result. a -> Control.Monad.Eff.Eff eff result
foreign import noop0 :: forall eff result. Control.Monad.Eff.Eff eff result