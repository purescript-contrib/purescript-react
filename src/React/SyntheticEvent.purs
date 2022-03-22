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

import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Prim.Row as Row
import Type.Proxy (Proxy(..))

type SyntheticEvent = SyntheticEvent_ (SyntheticEvent' ())

type SyntheticAnimationEvent = SyntheticEvent_ (SyntheticAnimationEvent' (SyntheticEvent' ()))

type SyntheticClipboardEvent = SyntheticEvent_ (SyntheticClipboardEvent' (SyntheticEvent' ()))

type SyntheticCompositionEvent = SyntheticEvent_ (SyntheticCompositionEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticInputEvent = SyntheticEvent_ (SyntheticUIEvent' (SyntheticEvent' ()))

type SyntheticKeyboardEvent = SyntheticEvent_ (SyntheticKeyboardEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticFocusEvent = SyntheticEvent_ (SyntheticFocusEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticMouseEvent = SyntheticEvent_ (SyntheticMouseEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticTouchEvent = SyntheticEvent_ (SyntheticTouchEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticTransitionEvent = SyntheticEvent_ (SyntheticTransitionEvent' (SyntheticEvent' ()))

type SyntheticUIEvent = SyntheticEvent_ (SyntheticUIEvent' (SyntheticEvent' ()))

type SyntheticWheelEvent = SyntheticEvent_ (SyntheticWheelEvent' (SyntheticMouseEvent' (SyntheticEvent' ())))

foreign import data SyntheticEvent_ :: Row Type -> Type

type role SyntheticEvent_ representational

foreign import data NativeEventTarget :: Type

foreign import data NativeEvent :: Type

foreign import data NativeDataTransfer :: Type

foreign import data NativeAbstractView :: Type

foreign import data NativeTouchList :: Type

type SyntheticEvent' r =
  ( bubbles :: Boolean
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

type SyntheticAnimationEvent' r =
  ( animationName :: String
  , pseudoElement :: String
  , elapsedTime :: Number
  | r
  )

type SyntheticClipboardEvent' r =
  ( clipboardData :: NativeDataTransfer
  | r
  )

type SyntheticCompositionEvent' r =
  ( data :: String
  | r
  )

type SyntheticFocusEvent' r =
  ( relatedTarget :: NativeEventTarget
  | r
  )

type SyntheticKeyboardEvent' r =
  ( altKey :: Boolean
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

type SyntheticMouseEvent' r =
  ( altKey :: Boolean
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

type SyntheticTouchEvent' r =
  ( altKey :: Boolean
  , changedTouches :: NativeTouchList
  , ctrlKey :: Boolean
  , getModifierState :: String -> Boolean
  , metaKey :: Boolean
  , targetTouches :: NativeTouchList
  , shiftKey :: Boolean
  , touches :: NativeTouchList
  | r
  )

type SyntheticTransitionEvent' r =
  ( propertyName :: String
  , pseudoElement :: String
  , elapsedTime :: Number
  | r
  )

type SyntheticUIEvent' r =
  ( detail :: Number
  , view :: NativeAbstractView
  | r
  )

type SyntheticWheelEvent' r =
  ( deltaMode :: Number
  , deltaX :: Number
  , deltaY :: Number
  , deltaZ :: Number
  | r
  )

bubbles :: forall r. SyntheticEvent_ (bubbles :: Boolean | r) -> Effect Boolean
bubbles = get (Proxy :: Proxy "bubbles")

cancelable :: forall r. SyntheticEvent_ (cancelable :: Boolean | r) -> Effect Boolean
cancelable = get (Proxy :: Proxy "cancelable")

currentTarget :: forall r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Effect NativeEventTarget
currentTarget = get (Proxy :: Proxy "currentTarget")

defaultPrevented :: forall r. SyntheticEvent_ (defaultPrevented :: Boolean | r) -> Effect Boolean
defaultPrevented = get (Proxy :: Proxy "defaultPrevented")

eventPhase :: forall r. SyntheticEvent_ (eventPhase :: Number | r) -> Effect Number
eventPhase = get (Proxy :: Proxy "eventPhase")

isTrusted :: forall r. SyntheticEvent_ (isTrusted :: Boolean | r) -> Effect Boolean
isTrusted = get (Proxy :: Proxy "isTrusted")

nativeEvent :: forall r. SyntheticEvent_ (nativeEvent :: NativeEvent | r) -> Effect NativeEvent
nativeEvent = get (Proxy :: Proxy "nativeEvent")

target :: forall r. SyntheticEvent_ (target :: NativeEventTarget | r) -> Effect NativeEventTarget
target = get (Proxy :: Proxy "target")

timeStamp :: forall r. SyntheticEvent_ (timeStamp :: Number | r) -> Effect Number
timeStamp = get (Proxy :: Proxy "timeStamp")

type_ :: forall r. SyntheticEvent_ (type :: String | r) -> Effect String
type_ = get (Proxy :: Proxy "type")

animationName :: forall r. SyntheticEvent_ (animationName :: String | r) -> Effect String
animationName = get (Proxy :: Proxy "animationName")

clipboardData :: forall r. SyntheticEvent_ (clipboardData :: NativeDataTransfer | r) -> Effect NativeDataTransfer
clipboardData = get (Proxy :: Proxy "clipboardData")

data_ :: forall r. SyntheticEvent_ (data :: String | r) -> Effect String
data_ = get (Proxy :: Proxy "data")

relatedTarget :: forall r. SyntheticEvent_ (relatedTarget :: NativeEventTarget | r) -> Effect NativeEventTarget
relatedTarget = get (Proxy :: Proxy "relatedTarget")

charCode :: forall r. SyntheticEvent_ (charCode :: Int | r) -> Effect Int
charCode = get (Proxy :: Proxy "charCode")

key :: forall r. SyntheticEvent_ (key :: String | r) -> Effect String
key = get (Proxy :: Proxy "key")

keyCode :: forall r. SyntheticEvent_ (keyCode :: Number | r) -> Effect Number
keyCode = get (Proxy :: Proxy "keyCode")

locale :: forall r. SyntheticEvent_ (locale :: String | r) -> Effect String
locale = get (Proxy :: Proxy "locale")

location :: forall r. SyntheticEvent_ (location :: Number | r) -> Effect Number
location = get (Proxy :: Proxy "location")

repeat :: forall r. SyntheticEvent_ (repeat :: Boolean | r) -> Effect Boolean
repeat = get (Proxy :: Proxy "repeat")

which :: forall r. SyntheticEvent_ (which :: Number | r) -> Effect Number
which = get (Proxy :: Proxy "which")

button :: forall r. SyntheticEvent_ (button :: Number | r) -> Effect Number
button = get (Proxy :: Proxy "button")

buttons :: forall r. SyntheticEvent_ (buttons :: Number | r) -> Effect Number
buttons = get (Proxy :: Proxy "buttons")

clientX :: forall r. SyntheticEvent_ (clientX :: Number | r) -> Effect Number
clientX = get (Proxy :: Proxy "clientX")

clientY :: forall r. SyntheticEvent_ (clientY :: Number | r) -> Effect Number
clientY = get (Proxy :: Proxy "clientY")

pageX :: forall r. SyntheticEvent_ (pageX :: Number | r) -> Effect Number
pageX = get (Proxy :: Proxy "pageX")

pageY :: forall r. SyntheticEvent_ (pageY :: Number | r) -> Effect Number
pageY = get (Proxy :: Proxy "pageY")

screenX :: forall r. SyntheticEvent_ (screenX :: Number | r) -> Effect Number
screenX = get (Proxy :: Proxy "screenX")

screenY :: forall r. SyntheticEvent_ (screenY :: Number | r) -> Effect Number
screenY = get (Proxy :: Proxy "screenY")

changedTouches :: forall r. SyntheticEvent_ (changedTouches :: NativeTouchList | r) -> Effect NativeTouchList
changedTouches = get (Proxy :: Proxy "changedTouches")

targetTouches :: forall r. SyntheticEvent_ (targetTouches :: NativeTouchList | r) -> Effect NativeTouchList
targetTouches = get (Proxy :: Proxy "targetTouches")

touches :: forall r. SyntheticEvent_ (touches :: NativeTouchList | r) -> Effect NativeTouchList
touches = get (Proxy :: Proxy "touches")

altKey :: forall r. SyntheticEvent_ (altKey :: Boolean | r) -> Effect Boolean
altKey = get (Proxy :: Proxy "altKey")

ctrlKey :: forall r. SyntheticEvent_ (ctrlKey :: Boolean | r) -> Effect Boolean
ctrlKey = get (Proxy :: Proxy "ctrlKey")

metaKey :: forall r. SyntheticEvent_ (metaKey :: Boolean | r) -> Effect Boolean
metaKey = get (Proxy :: Proxy "metaKey")

shiftKey :: forall r. SyntheticEvent_ (shiftKey :: Boolean | r) -> Effect Boolean
shiftKey = get (Proxy :: Proxy "shiftKey")

propertyName :: forall r. SyntheticEvent_ (propertyName :: String | r) -> Effect String
propertyName = get (Proxy :: Proxy "propertyName")

pseudoElement :: forall r. SyntheticEvent_ (pseudoElement :: String | r) -> Effect String
pseudoElement = get (Proxy :: Proxy "pseudoElement")

elapsedTime :: forall r. SyntheticEvent_ (elapsedTime :: Number | r) -> Effect Number
elapsedTime = get (Proxy :: Proxy "elapsedTime")

detail :: forall r. SyntheticEvent_ (detail :: Number | r) -> Effect Number
detail = get (Proxy :: Proxy "detail")

view :: forall r. SyntheticEvent_ (view :: NativeAbstractView | r) -> Effect NativeAbstractView
view = get (Proxy :: Proxy "view")

deltaMode :: forall r. SyntheticEvent_ (deltaMode :: Number | r) -> Effect Number
deltaMode = get (Proxy :: Proxy "deltaMode")

deltaX :: forall r. SyntheticEvent_ (deltaX :: Number | r) -> Effect Number
deltaX = get (Proxy :: Proxy "deltaX")

deltaY :: forall r. SyntheticEvent_ (deltaY :: Number | r) -> Effect Number
deltaY = get (Proxy :: Proxy "deltaY")

deltaZ :: forall r. SyntheticEvent_ (deltaZ :: Number | r) -> Effect Number
deltaZ = get (Proxy :: Proxy "deltaZ")

foreign import preventDefault :: forall r. SyntheticEvent_ r -> Effect Unit

foreign import isDefaultPrevented :: forall r. SyntheticEvent_ r -> Effect Boolean

foreign import stopPropagation :: forall r. SyntheticEvent_ r -> Effect Unit

foreign import isPropagationStopped :: forall r. SyntheticEvent_ r -> Effect Boolean

foreign import persist :: forall r. SyntheticEvent_ r -> Effect Unit

foreign import getModifierState
  :: forall r
   . String
  -> SyntheticEvent_ (getModifierState :: String -> Boolean | r)
  -> Effect Boolean

get
  :: forall l r s a
   . Row.Cons l a r s
  => IsSymbol l
  => Proxy l
  -> SyntheticEvent_ s
  -> Effect a
get l r = unsafeGet (reflectSymbol l) r

foreign import unsafeGet :: forall r a. String -> SyntheticEvent_ r -> Effect a
