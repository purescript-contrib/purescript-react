module React.Ref
  ( Ref
  , DOMRef
  , createRef
  , forwardRef
  , getRef
  , setRef
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

import React.Types (ReactClass, ReactElement)

-- | Type for React refs.
foreign import data Ref :: Type -> Type

-- | Type for a Ref value that is a React component instance or a DOM element.
-- | This type is opaque, but you can use `Data.Foreign` and `DOM` to validate the underlying representation.
foreign import data DOMRef :: Type

foreign import createRef :: forall a. Effect (Ref a)

forwardRef :: forall props a. (props -> Ref a -> ReactElement) -> ReactClass props
forwardRef = forwardRef_ <<< mkFn2

foreign import forwardRef_
  :: forall props a
   . Fn2 props (Ref a) ReactElement
  -> ReactClass props

getRef :: forall a. Ref a -> Effect (Maybe a)
getRef r = Nullable.toMaybe <$> runEffectFn1 getRef_ r

foreign import getRef_
  :: forall a
   . EffectFn1 (Ref a)
               (Nullable a)

setRef :: forall a. Ref a -> Maybe a -> Effect Unit
setRef r = runEffectFn2 setRef_ r <<< Nullable.toNullable

foreign import setRef_
  :: forall a
   . EffectFn2 (Ref a)
               (Nullable a)
               Unit
