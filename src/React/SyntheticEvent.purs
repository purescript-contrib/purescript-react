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
  , SyntheticFocusEvent'
  , SyntheticMouseEvent'
  , SyntheticTouchEvent'
  , SyntheticTransitionEvent'
  , SyntheticUIEvent'
  , SyntheticWheelEvent'

  , NativeEventTarget
  , NativeEvent
  , NativeDataTransfer
  , NativeAbstractView
  , NativeTouchList

  , preventDefault
  , isDefaultPrevented
  , stopPropagation
  , isPropagationStopped
  , persist
  ) where

import Prelude

import Control.Monad.Eff (Eff)

type SyntheticEvent = Record (SyntheticEvent' ())

type SyntheticAnimationEvent = Record (SyntheticAnimationEvent' (SyntheticEvent' ()))

type SyntheticClipboardEvent = Record (SyntheticClipboardEvent' (SyntheticEvent' ()))

type SyntheticCompositionEvent = Record (SyntheticCompositionEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticInputEvent = Record (SyntheticUIEvent' (SyntheticEvent' ()))

type SyntheticKeyboardEvent = Record (SyntheticKeyboardEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticFocusEvent = Record (SyntheticFocusEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticMouseEvent = Record (SyntheticMouseEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticTouchEvent = Record (SyntheticTouchEvent' (SyntheticUIEvent' (SyntheticEvent' ())))

type SyntheticTransitionEvent = Record (SyntheticTransitionEvent' (SyntheticEvent' ()))

type SyntheticUIEvent = Record (SyntheticUIEvent' (SyntheticEvent' ()))

type SyntheticWheelEvent = Record (SyntheticWheelEvent' (SyntheticMouseEvent' (SyntheticEvent' ())))

foreign import data NativeEventTarget :: Type

foreign import data NativeEvent :: Type

foreign import data NativeDataTransfer :: Type

foreign import data NativeAbstractView :: Type

foreign import data NativeTouchList :: Type

foreign import preventDefault :: forall eff r. Record (SyntheticEvent' r) -> Eff eff Unit

foreign import isDefaultPrevented :: forall eff r. Record (SyntheticEvent' r) -> Eff eff Boolean

foreign import stopPropagation :: forall eff r. Record (SyntheticEvent' r) -> Eff eff Unit

foreign import isPropagationStopped :: forall eff r. Record (SyntheticEvent' r) -> Eff eff Boolean

foreign import persist :: forall eff r. Record (SyntheticEvent' r) -> Eff eff Unit

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
    , charCode :: Int
    , ctrlKey :: Boolean
    , getModifierState :: String -> Boolean
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
    , shiftKey :: Boolean
    , targetTouches :: NativeTouchList
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
