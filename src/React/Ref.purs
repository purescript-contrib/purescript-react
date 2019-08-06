module React.Ref
  ( Ref
  , RefHandler
  , ReactInstance
  , NativeNode
  , fromRef
  , fromEffect
  , getCurrentRef
  , createDOMRef
  , createInstanceRef
  ) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Uncurried (EffectFn1, runEffectFn1, mkEffectFn1)
import Unsafe.Coerce (unsafeCoerce)


foreign import data ReactInstance :: Type

foreign import data NativeNode :: Type

foreign import data Ref :: Type -> Type

foreign import data RefHandler :: Type -> Type


foreign import createRef :: forall a. Effect (Ref a)

foreign import liftCallbackRef :: forall a. Ref a -> Ref a


createDOMRef :: Effect (Ref NativeNode)
createDOMRef = createRef


createInstanceRef :: Effect (Ref ReactInstance)
createInstanceRef = createRef


fromRef :: forall a. Ref a -> RefHandler a
fromRef = unsafeCoerce


fromEffect :: forall a. (Ref a -> Effect Unit) -> RefHandler a
fromEffect f = unsafeCoerce $ mkEffectFn1 (f <<< liftCallbackRef)


foreign import getCurrentRef_ :: forall a. EffectFn1 (Ref a) (Nullable a)

getCurrentRef :: forall a. Ref a -> Effect (Maybe a)
getCurrentRef ref = Nullable.toMaybe <$> runEffectFn1 getCurrentRef_ ref
