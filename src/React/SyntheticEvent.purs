-- | Synthethic event representation for React.
-- |
-- | https://reactjs.org/docs/events.html
-- | https://reactjs.org/docs/handling-events.html
-- | https://flow.org/en/docs/react/events/
-- |
module React.SyntheticEvent
  ( SyntheticEvent
  , SyntheticAnimationEvent
  , SyntheticClipboardEvent
  , SyntheticCompositionEvent
  , SyntheticInputEvent
  , SyntheticKeyboardEvent
  , SyntheticFocusEvent
  , SyntheticMouseEvent
  , SyntheticTouchEvent
  , SyntheticTransitionEvent
  , SyntheticUIEvent
  , SyntheticWheelEvent

  , SyntheticEvent'
  , SyntheticAnimationEvent'
  , SyntheticClipboardEvent'
  , SyntheticCompositionEvent'
  , SyntheticFocusEvent'
  , SyntheticKeyboardEvent'
  , SyntheticMouseEvent'
  , SyntheticTouchEvent'
  , SyntheticTransitionEvent'
  , SyntheticUIEvent'
  , SyntheticWheelEvent'

  , SyntheticEvent_

  , NativeEventTarget
  , NativeEvent
  , NativeDataTransfer
  , NativeAbstractView
  , NativeTouchList

  , bubbles
  , cancelable
  , currentTarget
  , defaultPrevented
  , eventPhase
  , isTrusted
  , nativeEvent
  , preventDefault
  , isDefaultPrevented
  , stopPropagation
  , isPropagationStopped
  , target
  , timeStamp
  , type_
  , persist
  , animationName
  , clipboardData
  , data_
  , relatedTarget
  , charCode
  , key
  , keyCode
  , locale
  , location
  , repeat
  , which
  , button
  , buttons
  , clientX
  , clientY
  , pageX
  , pageY
  , screenX
  , screenY
  , changedTouches
  , targetTouches
  , touches
  , altKey
  , ctrlKey
  , getModifierState
  , metaKey
  , shiftKey
  , propertyName
  , pseudoElement
  , elapsedTime
  , detail
  , view
  , deltaMode
  , deltaX
  , deltaY
  , deltaZ
  ) where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Prim.Row as Row

type SyntheticEvent
  = SyntheticEvent_ (SyntheticEvent' ())

type SyntheticAnimationEvent
  = SyntheticEvent_ (SyntheticAnimationEvent' (SyntheticEvent' ()))

type SyntheticClipboardEvent
  = SyntheticEvent_ (SyntheticClipboardEvent' (SyntheticEvent' ()))

type SyntheticCompositionEvent
  = SyntheticEvent_ (SyntheticCompositionEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticInputEvent
  = SyntheticEvent_ (SyntheticUIEvent' (SyntheticEvent' ()))

type SyntheticKeyboardEvent
  = SyntheticEvent_ (SyntheticKeyboardEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticFocusEvent
  = SyntheticEvent_ (SyntheticFocusEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticMouseEvent
  = SyntheticEvent_ (SyntheticMouseEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticTouchEvent
  = SyntheticEvent_ (SyntheticTouchEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticTransitionEvent
  = SyntheticEvent_ (SyntheticTransitionEvent' (SyntheticEvent' ()))

type SyntheticUIEvent
  = SyntheticEvent_ (SyntheticUIEvent' (SyntheticEvent' ()))

type SyntheticWheelEvent
  = SyntheticEvent_ (SyntheticWheelEvent' (SyntheticMouseEvent' (SyntheticEvent' ())))

foreign import data SyntheticEvent_ :: # Type -> Type

foreign import data NativeEventTarget :: Type

foreign import data NativeEvent :: Type

foreign import data NativeDataTransfer :: Type

foreign import data NativeAbstractView :: Type

foreign import data NativeTouchList :: Type

type SyntheticEvent' r
  = ( bubbles :: Boolean
    , cancelable :: Boolean
    , currentTarget :: NativeEventTarget
    , defaultPrevented :: Boolean
    , eventPhase :: Number
    , isTrusted :: Boolean
    , nativeEvent :: NativeEvent
    , target :: NativeEventTarget
    , timeStamp :: Number
    , type :: String
    | r
    )

type SyntheticAnimationEvent' r
  = ( animationName :: String
    , pseudoElement :: String
    , elapsedTime :: Number
    | r
    )

type SyntheticClipboardEvent' r
  = ( clipboardData :: NativeDataTransfer
    | r
    )

type SyntheticCompositionEvent' r
  = ( data :: String
    | r
    )

type SyntheticFocusEvent'  r
  = ( relatedTarget :: NativeEventTarget
    | r
    )

type SyntheticKeyboardEvent' r
  = ( altKey :: Boolean
    , ctrlKey :: Boolean
    , getModifierState :: String -> Boolean
    , charCode :: Int
    , key :: String
    , keyCode :: Number
    , locale :: String
    , location :: Number
    , metaKey :: Boolean
    , repeat :: Boolean
    , shiftKey :: Boolean
    , which :: Number
    | r
    )

type SyntheticMouseEvent' r
  = ( altKey :: Boolean
    , button :: Number
    , buttons :: Number
    , clientX :: Number
    , clientY :: Number
    , ctrlKey :: Boolean
    , getModifierState :: String -> Boolean
    , metaKey :: Boolean
    , pageX :: Number
    , pageY :: Number
    , relatedTarget :: NativeEventTarget
    , screenX :: Number
    , screenY :: Number
    , shiftKey :: Boolean
    | r
    )

type SyntheticTouchEvent' r
  = ( altKey :: Boolean
    , changedTouches :: NativeTouchList
    , ctrlKey :: Boolean
    , getModifierState :: String -> Boolean
    , metaKey :: Boolean
    , targetTouches :: NativeTouchList
    , shiftKey :: Boolean
    , touches :: NativeTouchList
    | r
    )

type SyntheticTransitionEvent' r
  = ( propertyName :: String
    , pseudoElement :: String
    , elapsedTime :: Number
    | r
    )

type SyntheticUIEvent' r
  = ( detail :: Number
    , view :: NativeAbstractView
    | r
    )

type SyntheticWheelEvent' r
  = ( deltaMode :: Number
    , deltaX :: Number
    , deltaY :: Number
    , deltaZ :: Number
    | r
    )

bubbles :: forall r. SyntheticEvent_ (bubbles :: Boolean | r) -> Effect Boolean
bubbles = get (SProxy :: SProxy "bubbles")

cancelable :: forall r. SyntheticEvent_ (cancelable :: Boolean | r) -> Effect Boolean
cancelable = get (SProxy :: SProxy "cancelable")

currentTarget :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Effect NativeEventTarget
currentTarget = get (SProxy :: SProxy "currentTarget")

defaultPrevented :: forall r. SyntheticEvent_ (defaultPrevented :: Boolean | r) -> Effect Boolean
defaultPrevented = get (SProxy :: SProxy "defaultPrevented")

eventPhase :: forall r. SyntheticEvent_ (eventPhase :: Number | r) -> Effect Number
eventPhase = get (SProxy :: SProxy "eventPhase")

isTrusted :: forall r. SyntheticEvent_ (isTrusted :: Boolean | r) -> Effect Boolean
isTrusted = get (SProxy :: SProxy "isTrusted")

nativeEvent :: forall r. SyntheticEvent_ (nativeEvent :: NativeEvent | r) -> Effect NativeEvent
nativeEvent = get (SProxy :: SProxy "nativeEvent")

target :: forall r. SyntheticEvent_ (target :: NativeEventTarget | r) -> Effect NativeEventTarget
target = get (SProxy :: SProxy "target")

timeStamp :: forall r. SyntheticEvent_ (timeStamp :: Number | r) -> Effect Number
timeStamp = get (SProxy :: SProxy "timeStamp")

type_ :: forall r. SyntheticEvent_ (type :: String | r) -> Effect String
type_ = get (SProxy :: SProxy "type")

animationName :: forall r. SyntheticEvent_ (animationName :: String | r) -> Effect String
animationName = get (SProxy :: SProxy "animationName")

clipboardData :: forall r. SyntheticEvent_ (clipboardData :: NativeDataTransfer | r) -> Effect NativeDataTransfer
clipboardData = get (SProxy :: SProxy "clipboardData")

data_ :: forall r. SyntheticEvent_ (data :: String | r) -> Effect String
data_ = get (SProxy :: SProxy "data")

relatedTarget :: forall r. SyntheticEvent_ (relatedTarget :: NativeEventTarget | r) -> Effect NativeEventTarget
relatedTarget = get (SProxy :: SProxy "relatedTarget")

charCode :: forall r. SyntheticEvent_ (charCode :: Int | r) -> Effect Int
charCode = get (SProxy :: SProxy "charCode")

key :: forall r. SyntheticEvent_ (key :: String | r) -> Effect String
key = get (SProxy :: SProxy "key")

keyCode :: forall r. SyntheticEvent_ (keyCode :: Number | r) -> Effect Number
keyCode = get (SProxy :: SProxy "keyCode")

locale :: forall r. SyntheticEvent_ (locale :: String | r) -> Effect String
locale = get (SProxy :: SProxy "locale")

location :: forall r. SyntheticEvent_ (location :: Number | r) -> Effect Number
location = get (SProxy :: SProxy "location")

repeat :: forall r. SyntheticEvent_ (repeat :: Boolean | r) -> Effect Boolean
repeat = get (SProxy :: SProxy "repeat")

which :: forall r. SyntheticEvent_ (which :: Number | r) -> Effect Number
which = get (SProxy :: SProxy "which")

button :: forall r. SyntheticEvent_ (button :: Number | r) -> Effect Number
button = get (SProxy :: SProxy "button")

buttons :: forall r. SyntheticEvent_ (buttons :: Number | r) -> Effect Number
buttons = get (SProxy :: SProxy "buttons")

clientX :: forall r. SyntheticEvent_ (clientX :: Number | r) -> Effect Number
clientX = get (SProxy :: SProxy "clientX")

clientY :: forall r. SyntheticEvent_ (clientY :: Number | r) -> Effect Number
clientY = get (SProxy :: SProxy "clientY")

pageX :: forall r. SyntheticEvent_ (pageX :: Number | r) -> Effect Number
pageX = get (SProxy :: SProxy "pageX")

pageY :: forall r. SyntheticEvent_ (pageY :: Number | r) -> Effect Number
pageY = get (SProxy :: SProxy "pageY")

screenX :: forall r. SyntheticEvent_ (screenX :: Number | r) -> Effect Number
screenX = get (SProxy :: SProxy "screenX")

screenY :: forall r. SyntheticEvent_ (screenY :: Number | r) -> Effect Number
screenY = get (SProxy :: SProxy "screenY")

changedTouches :: forall r. SyntheticEvent_ (changedTouches :: NativeTouchList | r) -> Effect NativeTouchList
changedTouches = get (SProxy :: SProxy "changedTouches")

targetTouches :: forall r. SyntheticEvent_ (targetTouches :: NativeTouchList | r) -> Effect NativeTouchList
targetTouches = get (SProxy :: SProxy "targetTouches")

touches :: forall r. SyntheticEvent_ (touches :: NativeTouchList | r) -> Effect NativeTouchList
touches = get (SProxy :: SProxy "touches")

altKey :: forall r. SyntheticEvent_ (altKey :: Boolean | r) -> Effect Boolean
altKey = get (SProxy :: SProxy "altKey")

ctrlKey :: forall r. SyntheticEvent_ (ctrlKey :: Boolean | r) -> Effect Boolean
ctrlKey = get (SProxy :: SProxy "ctrlKey")

metaKey :: forall r. SyntheticEvent_ (metaKey :: Boolean | r) -> Effect Boolean
metaKey = get (SProxy :: SProxy "metaKey")

shiftKey :: forall r. SyntheticEvent_ (shiftKey :: Boolean | r) -> Effect Boolean
shiftKey = get (SProxy :: SProxy "shiftKey")

propertyName :: forall r. SyntheticEvent_ (propertyName :: String | r) -> Effect String
propertyName = get (SProxy :: SProxy "propertyName")

pseudoElement :: forall r. SyntheticEvent_ (pseudoElement :: String | r) -> Effect String
pseudoElement = get (SProxy :: SProxy "pseudoElement")

elapsedTime :: forall r. SyntheticEvent_ (elapsedTime :: Number | r) -> Effect Number
elapsedTime = get (SProxy :: SProxy "elapsedTime")

detail :: forall r. SyntheticEvent_ (detail :: Number | r) -> Effect Number
detail = get (SProxy :: SProxy "detail")

view :: forall r. SyntheticEvent_ (view :: NativeAbstractView | r) -> Effect NativeAbstractView
view = get (SProxy :: SProxy "view")

deltaMode :: forall r. SyntheticEvent_ (deltaMode :: Number | r) -> Effect Number
deltaMode = get (SProxy :: SProxy "deltaMode")

deltaX :: forall r. SyntheticEvent_ (deltaX :: Number | r) -> Effect Number
deltaX = get (SProxy :: SProxy "deltaX")

deltaY :: forall r. SyntheticEvent_ (deltaY :: Number | r) -> Effect Number
deltaY = get (SProxy :: SProxy "deltaY")

deltaZ :: forall r. SyntheticEvent_ (deltaZ :: Number | r) -> Effect Number
deltaZ = get (SProxy :: SProxy "deltaZ")

foreign import preventDefault :: forall r. SyntheticEvent_ r -> Effect Unit

foreign import isDefaultPrevented :: forall r. SyntheticEvent_ r -> Effect Boolean

foreign import stopPropagation :: forall r. SyntheticEvent_ r -> Effect Unit

foreign import isPropagationStopped :: forall r. SyntheticEvent_ r -> Effect Boolean

foreign import persist :: forall r. SyntheticEvent_ r -> Effect Unit

foreign import getModifierState :: forall r. String -> SyntheticEvent_ (getModifierState :: String -> Boolean | r) -> Effect Boolean

get
  :: forall l r s a
   . Row.Cons l a r s
  => IsSymbol l
  => SProxy l
  -> SyntheticEvent_ s
  -> Effect a
get l r = unsafeGet (reflectSymbol l) r

foreign import unsafeGet :: forall r a. String -> SyntheticEvent_ r -> Effect a
