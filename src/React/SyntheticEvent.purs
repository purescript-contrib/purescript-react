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
  , SyntheticKeyboardEvent'
  , SyntheticMouseEvent'
  , SyntheticTouchEvent'
  , SyntheticTransitionEvent'
  , SyntheticUIEvent'
  , SyntheticWheelEvent'

  , SyntheticAnimationTransitionEvent'
  , SyntheticKeyboardMouseTouchEvent'
  , SyntheticFocusMouseEvent'

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

type SyntheticEvent r
  = SyntheticEvent_ (SyntheticEvent' r)

type SyntheticAnimationEvent r
  = SyntheticEvent_ (SyntheticAnimationEvent' (SyntheticAnimationTransitionEvent' (SyntheticEvent' r)))

type SyntheticClipboardEvent r
  = SyntheticEvent_ (SyntheticClipboardEvent' (SyntheticEvent' r))

type SyntheticCompositionEvent r
  = SyntheticEvent_ (SyntheticCompositionEvent' (SyntheticUIEvent' (SyntheticEvent' r)))

type SyntheticInputEvent r
  = SyntheticEvent_ (SyntheticUIEvent' (SyntheticEvent' r))

type SyntheticKeyboardEvent r
  = SyntheticEvent_ (SyntheticKeyboardEvent' (SyntheticKeyboardMouseTouchEvent' (SyntheticUIEvent' (SyntheticEvent' r))))

type SyntheticFocusEvent r
  = SyntheticEvent_ (SyntheticFocusMouseEvent' (SyntheticUIEvent' (SyntheticEvent' r)))

type SyntheticMouseEvent r
  = SyntheticEvent_ (SyntheticMouseEvent' (SyntheticFocusMouseEvent' (SyntheticKeyboardMouseTouchEvent' (SyntheticUIEvent' (SyntheticEvent' r)))))

type SyntheticTouchEvent r
  = SyntheticEvent_ (SyntheticTouchEvent' (SyntheticKeyboardMouseTouchEvent' (SyntheticUIEvent' (SyntheticEvent' r))))

type SyntheticTransitionEvent r
  = SyntheticEvent_ (SyntheticTransitionEvent' (SyntheticAnimationTransitionEvent' (SyntheticEvent' r)))

type SyntheticUIEvent r
  = SyntheticEvent_ (SyntheticUIEvent' (SyntheticEvent' r))

type SyntheticWheelEvent r
  = SyntheticEvent_ (SyntheticWheelEvent' (SyntheticMouseEvent' (SyntheticEvent' r)))

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

type SyntheticKeyboardEvent' r
  = ( charCode :: Int
    , key :: String
    , keyCode :: Number
    , locale :: String
    , location :: Number
    , repeat :: Boolean
    , which :: Number
    | r
    )

type SyntheticMouseEvent' r
  = ( button :: Number
    , buttons :: Number
    , clientX :: Number
    , clientY :: Number
    , pageX :: Number
    , pageY :: Number
    , relatedTarget :: NativeEventTarget
    , screenX :: Number
    , screenY :: Number
    | r
    )

type SyntheticTouchEvent' r
  = ( changedTouches :: NativeTouchList
    , targetTouches :: NativeTouchList
    , touches :: NativeTouchList
    | r
    )

type SyntheticTransitionEvent' r
  = ( propertyName :: String
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

type SyntheticAnimationTransitionEvent' r
  = ( pseudoElement :: String
    , elapsedTime :: Number
    | r
    )

type SyntheticFocusMouseEvent'  r
  = ( relatedTarget :: NativeEventTarget
    | r
    )

type SyntheticKeyboardMouseTouchEvent' r
  = ( altKey :: Boolean
    , ctrlKey :: Boolean
    , metaKey :: Boolean
    , shiftKey :: Boolean
    | r
    )

bubbles :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Boolean
bubbles = get (SProxy :: SProxy "bubbles")

cancelable :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Boolean
cancelable = get (SProxy :: SProxy "cancelable")

currentTarget :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff NativeEventTarget
currentTarget = get (SProxy :: SProxy "currentTarget")

defaultPrevented :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Boolean
defaultPrevented = get (SProxy :: SProxy "defaultPrevented")

eventPhase :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Number
eventPhase = get (SProxy :: SProxy "eventPhase")

isTrusted :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Boolean
isTrusted = get (SProxy :: SProxy "isTrusted")

nativeEvent :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff NativeEvent
nativeEvent = get (SProxy :: SProxy "nativeEvent")

target :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff NativeEventTarget
target = get (SProxy :: SProxy "target")

timeStamp :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Number
timeStamp = get (SProxy :: SProxy "timeStamp")

type_ :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff String
type_ = get (SProxy :: SProxy "type")

animationName :: forall eff r. SyntheticEvent_ (SyntheticAnimationEvent' r) -> Eff eff String
animationName = get (SProxy :: SProxy "animationName")

clipboardData :: forall eff r. SyntheticEvent_ (SyntheticClipboardEvent' r) -> Eff eff NativeDataTransfer
clipboardData = get (SProxy :: SProxy "clipboardData")

data_ :: forall eff r. SyntheticEvent_ (SyntheticCompositionEvent' r) -> Eff eff String
data_ = get (SProxy :: SProxy "data")

relatedTarget :: forall eff r. SyntheticEvent_ (SyntheticFocusMouseEvent' r) -> Eff eff NativeEventTarget
relatedTarget = get (SProxy :: SProxy "relatedTarget")

charCode :: forall eff r. SyntheticEvent_ (SyntheticKeyboardEvent' r) -> Eff eff Int
charCode = get (SProxy :: SProxy "charCode")

key :: forall eff r. SyntheticEvent_ (SyntheticKeyboardEvent' r) -> Eff eff String
key = get (SProxy :: SProxy "key")

keyCode :: forall eff r. SyntheticEvent_ (SyntheticKeyboardEvent' r) -> Eff eff Number
keyCode = get (SProxy :: SProxy "keyCode")

locale :: forall eff r. SyntheticEvent_ (SyntheticKeyboardEvent' r) -> Eff eff String
locale = get (SProxy :: SProxy "locale")

location :: forall eff r. SyntheticEvent_ (SyntheticKeyboardEvent' r) -> Eff eff Number
location = get (SProxy :: SProxy "location")

repeat :: forall eff r. SyntheticEvent_ (SyntheticKeyboardEvent' r) -> Eff eff Boolean
repeat = get (SProxy :: SProxy "repeat")

which :: forall eff r. SyntheticEvent_ (SyntheticKeyboardEvent' r) -> Eff eff Number
which = get (SProxy :: SProxy "which")

button :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
button = get (SProxy :: SProxy "button")

buttons :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
buttons = get (SProxy :: SProxy "buttons")

clientX :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
clientX = get (SProxy :: SProxy "clientX")

clientY :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
clientY = get (SProxy :: SProxy "clientY")

pageX :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
pageX = get (SProxy :: SProxy "pageX")

pageY :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
pageY = get (SProxy :: SProxy "pageY")

screenX :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
screenX = get (SProxy :: SProxy "screenX")

screenY :: forall eff r. SyntheticEvent_ (SyntheticMouseEvent' r) -> Eff eff Number
screenY = get (SProxy :: SProxy "screenY")

changedTouches :: forall eff r. SyntheticEvent_ (SyntheticTouchEvent' r) -> Eff eff NativeTouchList
changedTouches = get (SProxy :: SProxy "changedTouches")

targetTouches :: forall eff r. SyntheticEvent_ (SyntheticTouchEvent' r) -> Eff eff NativeTouchList
targetTouches = get (SProxy :: SProxy "targetTouches")

touches :: forall eff r. SyntheticEvent_ (SyntheticTouchEvent' r) -> Eff eff NativeTouchList
touches = get (SProxy :: SProxy "touches")

altKey :: forall eff r. SyntheticEvent_ (SyntheticKeyboardMouseTouchEvent' r) -> Eff eff Boolean
altKey = get (SProxy :: SProxy "altKey")

ctrlKey :: forall eff r. SyntheticEvent_ (SyntheticKeyboardMouseTouchEvent' r) -> Eff eff Boolean
ctrlKey = get (SProxy :: SProxy "ctrlKey")

metaKey :: forall eff r. SyntheticEvent_ (SyntheticKeyboardMouseTouchEvent' r) -> Eff eff Boolean
metaKey = get (SProxy :: SProxy "metaKey")

shiftKey :: forall eff r. SyntheticEvent_ (SyntheticKeyboardMouseTouchEvent' r) -> Eff eff Boolean
shiftKey = get (SProxy :: SProxy "shiftKey")

propertyName :: forall eff r. SyntheticEvent_ (SyntheticTransitionEvent' r) -> Eff eff String
propertyName = get (SProxy :: SProxy "propertyName")

pseudoElement :: forall eff r. SyntheticEvent_ (SyntheticAnimationTransitionEvent' r) -> Eff eff String
pseudoElement = get (SProxy :: SProxy "pseudoElement")

elapsedTime :: forall eff r. SyntheticEvent_ (SyntheticAnimationTransitionEvent' r) -> Eff eff Number
elapsedTime = get (SProxy :: SProxy "elapsedTime")

detail :: forall eff r. SyntheticEvent_ (SyntheticUIEvent' r) -> Eff eff Number
detail = get (SProxy :: SProxy "detail")

view :: forall eff r. SyntheticEvent_ (SyntheticUIEvent' r) -> Eff eff NativeAbstractView
view = get (SProxy :: SProxy "view")

deltaMode :: forall eff r. SyntheticEvent_ (SyntheticWheelEvent' r) -> Eff eff Number
deltaMode = get (SProxy :: SProxy "deltaMode")

deltaX :: forall eff r. SyntheticEvent_ (SyntheticWheelEvent' r) -> Eff eff Number
deltaX = get (SProxy :: SProxy "deltaX")

deltaY :: forall eff r. SyntheticEvent_ (SyntheticWheelEvent' r) -> Eff eff Number
deltaY = get (SProxy :: SProxy "deltaY")

deltaZ :: forall eff r. SyntheticEvent_ (SyntheticWheelEvent' r) -> Eff eff Number
deltaZ = get (SProxy :: SProxy "deltaZ")

foreign import preventDefault :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Unit

foreign import isDefaultPrevented :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Boolean

foreign import stopPropagation :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Unit

foreign import isPropagationStopped :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Boolean

foreign import persist :: forall eff r. SyntheticEvent_ (SyntheticEvent' r) -> Eff eff Unit

foreign import getModifierState :: forall eff r. String -> SyntheticEvent_ (SyntheticKeyboardMouseTouchEvent' r) -> Eff eff Boolean

get
  :: forall eff l r s a
   . RowCons l a r s
  => IsSymbol l
  => SProxy l
  -> SyntheticEvent_ s
  -> Eff eff a
get l r = unsafeGet (reflectSymbol l) r

foreign import unsafeGet :: forall eff r a. String -> SyntheticEvent_ r -> Eff eff a
