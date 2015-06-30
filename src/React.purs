module React where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

foreign import data DOM :: !
foreign import data UI :: *
foreign import data UIRef :: # ! -> *
foreign import data EventHandler :: * -> *

foreign import data Disallowed :: !
foreign import data ReadAllowed :: !
foreign import data WriteAllowed :: !

foreign import data ReactState :: # ! -> * -> !
foreign import data ReactProps :: * -> !
foreign import data ReactRefs :: * -> !

foreign import noop0 :: forall eff result. Eff ( eff ) result

foreign import noop1 :: forall a eff result. a -> Eff ( eff ) result

foreign import noop2 :: forall a b eff result. a -> b -> Eff ( eff ) result

type Render props refs state eff =
  Eff (
    props :: ReactProps props,
    refs :: Disallowed,
    state :: ReactState (read :: ReadAllowed) state
    | eff
  ) UI

type UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8 =
  { getInitialState
      :: Eff (
        props :: ReactProps props,
        state :: Disallowed,
        refs :: Disallowed
        | eff1
      ) state
  , componentWillMount
      :: Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state,
        refs :: Disallowed
        | eff2
      ) Unit
  , componentDidMount
      :: Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state,
        refs :: ReactRefs refs
        | eff3
      ) Unit
  , componentWillReceiveProps
      :: props
      -> Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state,
        refs :: ReactRefs refs
        | eff4
      ) Unit
  , shouldComponentUpdate
      :: props
      -> state
      -> Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state,
        refs :: ReactRefs refs
        | eff5
      ) Boolean
  , componentWillUpdate
      :: props
      -> state
      -> Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state,
        refs :: ReactRefs refs
        | eff6
      ) Unit
  , componentDidUpdate
      :: props
      -> state
      -> Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed) state,
        refs :: ReactRefs refs
        | eff7
      ) Unit
  , componentWillUnmount
      :: Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed) state,
        refs :: ReactRefs refs
        | eff8
      ) Unit
  }

spec
  :: forall props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8.
  UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8
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
  updateAlways
      :: forall props refs state. props
      -> state
      -> forall eff. Eff (
        props :: ReactProps props,
        state :: ReactState (read :: ReadAllowed, write :: WriteAllowed) state,
        refs :: ReactRefs refs
        | eff
      ) Boolean
  updateAlways props state = return true

foreign import getProps :: forall props eff. Eff (props :: ReactProps props | eff) props

foreign import getRefs :: forall refs eff. Eff (refs :: ReactRefs refs | eff) refs

foreign import writeState :: forall state statePerms eff. 
                               state -> Eff (state :: ReactState (read :: ReadAllowed, write :: WriteAllowed | statePerms) state | eff) state

foreign import readState :: forall state statePerms eff. Eff (state :: ReactState (read :: ReadAllowed | statePerms) state | eff) state

transformState f = do
  state <- readState
  writeState $ f state

foreign import getSelf :: forall eff. Eff (eff) (UIRef eff)

foreign import runUI :: forall refEff eff result. UIRef refEff -> Eff (refEff) result -> Eff (eff) result

foreign import mkUI :: forall props refs state eff eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8.
                         UISpec props refs state eff1 eff2 eff3 eff4 eff5 eff6 eff7 eff8
                         -> Render props refs state eff
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

type EventHandlerContext eff props refs state result = forall statePerms. Eff (
  props :: ReactProps props,
  refs :: ReactRefs refs,
  state :: ReactState (read :: ReadAllowed, write :: WriteAllowed | statePerms) state
  | eff
  ) result

foreign import handle :: forall eff ev props refs state result.
                           (ev -> EventHandlerContext eff props refs state result)
                           -> EventHandler ev

foreign import renderToString :: UI -> String

foreign import renderToBody :: forall eff. UI -> Eff (dom :: DOM | eff) UI

foreign import renderToElementById :: forall eff. String -> UI -> Eff (dom :: DOM | eff) UI

foreign import deferred :: forall a eff. Eff eff a -> Eff eff a
