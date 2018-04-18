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

import Control.Monad.Eff (Eff)

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

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

bubbles :: forall eff r. SyntheticEvent_ (bubbles :: Boolean | r) -> Eff eff Boolean
bubbles = get (SProxy :: SProxy "bubbles")

cancelable :: forall eff r. SyntheticEvent_ (cancelable :: Boolean | r) -> Eff eff Boolean
cancelable = get (SProxy :: SProxy "cancelable")

currentTarget :: forall eff r. SyntheticEvent_ (currentTarget :: NativeEventTarget | r) -> Eff eff NativeEventTarget
currentTarget = get (SProxy :: SProxy "currentTarget")

defaultPrevented :: forall eff r. SyntheticEvent_ (defaultPrevented :: Boolean | r) -> Eff eff Boolean
defaultPrevented = get (SProxy :: SProxy "defaultPrevented")

eventPhase :: forall eff r. SyntheticEvent_ (eventPhase :: Number | r) -> Eff eff Number
eventPhase = get (SProxy :: SProxy "eventPhase")

isTrusted :: forall eff r. SyntheticEvent_ (isTrusted :: Boolean | r) -> Eff eff Boolean
isTrusted = get (SProxy :: SProxy "isTrusted")

nativeEvent :: forall eff r. SyntheticEvent_ (nativeEvent :: NativeEvent | r) -> Eff eff NativeEvent
nativeEvent = get (SProxy :: SProxy "nativeEvent")

target :: forall eff r. SyntheticEvent_ (target :: NativeEventTarget | r) -> Eff eff NativeEventTarget
target = get (SProxy :: SProxy "target")

timeStamp :: forall eff r. SyntheticEvent_ (timeStamp :: Number | r) -> Eff eff Number
timeStamp = get (SProxy :: SProxy "timeStamp")

type_ :: forall eff r. SyntheticEvent_ (type :: String | r) -> Eff eff String
type_ = get (SProxy :: SProxy "type")

animationName :: forall eff r. SyntheticEvent_ (animationName :: String | r) -> Eff eff String
animationName = get (SProxy :: SProxy "animationName")

clipboardData :: forall eff r. SyntheticEvent_ (clipboardData :: NativeDataTransfer | r) -> Eff eff NativeDataTransfer
clipboardData = get (SProxy :: SProxy "clipboardData")

data_ :: forall eff r. SyntheticEvent_ (data :: String | r) -> Eff eff String
data_ = get (SProxy :: SProxy "data")

relatedTarget :: forall eff r. SyntheticEvent_ (relatedTarget :: NativeEventTarget | r) -> Eff eff NativeEventTarget
relatedTarget = get (SProxy :: SProxy "relatedTarget")

charCode :: forall eff r. SyntheticEvent_ (charCode :: Int | r) -> Eff eff Int
charCode = get (SProxy :: SProxy "charCode")

key :: forall eff r. SyntheticEvent_ (key :: String | r) -> Eff eff String
key = get (SProxy :: SProxy "key")

keyCode :: forall eff r. SyntheticEvent_ (keyCode :: Number | r) -> Eff eff Number
keyCode = get (SProxy :: SProxy "keyCode")

locale :: forall eff r. SyntheticEvent_ (locale :: String | r) -> Eff eff String
locale = get (SProxy :: SProxy "locale")

location :: forall eff r. SyntheticEvent_ (location :: Number | r) -> Eff eff Number
location = get (SProxy :: SProxy "location")

repeat :: forall eff r. SyntheticEvent_ (repeat :: Boolean | r) -> Eff eff Boolean
repeat = get (SProxy :: SProxy "repeat")

which :: forall eff r. SyntheticEvent_ (which :: Number | r) -> Eff eff Number
which = get (SProxy :: SProxy "which")

button :: forall eff r. SyntheticEvent_ (button :: Number | r) -> Eff eff Number
button = get (SProxy :: SProxy "button")

buttons :: forall eff r. SyntheticEvent_ (buttons :: Number | r) -> Eff eff Number
buttons = get (SProxy :: SProxy "buttons")

clientX :: forall eff r. SyntheticEvent_ (clientX :: Number | r) -> Eff eff Number
clientX = get (SProxy :: SProxy "clientX")

clientY :: forall eff r. SyntheticEvent_ (clientY :: Number | r) -> Eff eff Number
clientY = get (SProxy :: SProxy "clientY")

pageX :: forall eff r. SyntheticEvent_ (pageX :: Number | r) -> Eff eff Number
pageX = get (SProxy :: SProxy "pageX")

pageY :: forall eff r. SyntheticEvent_ (pageY :: Number | r) -> Eff eff Number
pageY = get (SProxy :: SProxy "pageY")

screenX :: forall eff r. SyntheticEvent_ (screenX :: Number | r) -> Eff eff Number
screenX = get (SProxy :: SProxy "screenX")

screenY :: forall eff r. SyntheticEvent_ (screenY :: Number | r) -> Eff eff Number
screenY = get (SProxy :: SProxy "screenY")

changedTouches :: forall eff r. SyntheticEvent_ (changedTouches :: NativeTouchList | r) -> Eff eff NativeTouchList
changedTouches = get (SProxy :: SProxy "changedTouches")

targetTouches :: forall eff r. SyntheticEvent_ (targetTouches :: NativeTouchList | r) -> Eff eff NativeTouchList
targetTouches = get (SProxy :: SProxy "targetTouches")

touches :: forall eff r. SyntheticEvent_ (touches :: NativeTouchList | r) -> Eff eff NativeTouchList
touches = get (SProxy :: SProxy "touches")

altKey :: forall eff r. SyntheticEvent_ (altKey :: Boolean | r) -> Eff eff Boolean
altKey = get (SProxy :: SProxy "altKey")

ctrlKey :: forall eff r. SyntheticEvent_ (ctrlKey :: Boolean | r) -> Eff eff Boolean
ctrlKey = get (SProxy :: SProxy "ctrlKey")

metaKey :: forall eff r. SyntheticEvent_ (metaKey :: Boolean | r) -> Eff eff Boolean
metaKey = get (SProxy :: SProxy "metaKey")

shiftKey :: forall eff r. SyntheticEvent_ (shiftKey :: Boolean | r) -> Eff eff Boolean
shiftKey = get (SProxy :: SProxy "shiftKey")

propertyName :: forall eff r. SyntheticEvent_ (propertyName :: String | r) -> Eff eff String
propertyName = get (SProxy :: SProxy "propertyName")

pseudoElement :: forall eff r. SyntheticEvent_ (pseudoElement :: String | r) -> Eff eff String
pseudoElement = get (SProxy :: SProxy "pseudoElement")

elapsedTime :: forall eff r. SyntheticEvent_ (elapsedTime :: Number | r) -> Eff eff Number
elapsedTime = get (SProxy :: SProxy "elapsedTime")

detail :: forall eff r. SyntheticEvent_ (detail :: Number | r) -> Eff eff Number
detail = get (SProxy :: SProxy "detail")

view :: forall eff r. SyntheticEvent_ (view :: NativeAbstractView | r) -> Eff eff NativeAbstractView
view = get (SProxy :: SProxy "view")

deltaMode :: forall eff r. SyntheticEvent_ (deltaMode :: Number | r) -> Eff eff Number
deltaMode = get (SProxy :: SProxy "deltaMode")

deltaX :: forall eff r. SyntheticEvent_ (deltaX :: Number | r) -> Eff eff Number
deltaX = get (SProxy :: SProxy "deltaX")

deltaY :: forall eff r. SyntheticEvent_ (deltaY :: Number | r) -> Eff eff Number
deltaY = get (SProxy :: SProxy "deltaY")

deltaZ :: forall eff r. SyntheticEvent_ (deltaZ :: Number | r) -> Eff eff Number
deltaZ = get (SProxy :: SProxy "deltaZ")

foreign import preventDefault :: forall eff r. SyntheticEvent_ r -> Eff eff Unit

foreign import isDefaultPrevented :: forall eff r. SyntheticEvent_ r -> Eff eff Boolean

foreign import stopPropagation :: forall eff r. SyntheticEvent_ r -> Eff eff Unit

foreign import isPropagationStopped :: forall eff r. SyntheticEvent_ r -> Eff eff Boolean

foreign import persist :: forall eff r. SyntheticEvent_ r -> Eff eff Unit

foreign import getModifierState :: forall eff r. String -> SyntheticEvent_ (getModifierState :: String -> Boolean | r) -> Eff eff Boolean

get
  :: forall eff l r s a
   . RowCons l a r s
  => IsSymbol l
  => SProxy l
  -> SyntheticEvent_ s
  -> Eff eff a
get l r = unsafeGet (reflectSymbol l) r

foreign import unsafeGet :: forall eff r a. String -> SyntheticEvent_ r -> Eff eff a
