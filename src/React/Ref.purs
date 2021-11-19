module React.Ref
  ( Ref
  , RefHandler
  , ReactInstance
  , NativeNode
  , fromRef
  , fromEffect
  , getCurrentRef
  , createNodeRef
  , createInstanceRef
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Unsafe.Coerce (unsafeCoerce)

--- | An instance of a React class.
foreign import data ReactInstance :: Type

--- | A platform-specific native layout node. On the web this will be a DOM
--- | element (see `Web.HTML.HTMLElement`).
foreign import data NativeNode :: Type

foreign import data Ref :: Type -> Type

type role Ref representational

foreign import data RefHandler :: Type -> Type

type role RefHandler representational

foreign import createRef :: forall a. Effect (Ref a)

foreign import liftCallbackRef :: forall a. Ref a -> Ref a

createNodeRef :: Effect (Ref NativeNode)
createNodeRef = createRef

createInstanceRef :: Effect (Ref ReactInstance)
createInstanceRef = createRef

fromRef :: forall a. Ref a -> RefHandler a
fromRef = unsafeCoerce

fromEffect :: forall a. (Ref a -> Effect Unit) -> RefHandler a
fromEffect f = unsafeCoerce $ mkEffectFn1 (f <<< liftCallbackRef)

foreign import getCurrentRef_ :: forall a. EffectFn1 (Ref a) (Nullable a)

getCurrentRef :: forall a. Ref a -> Effect (Maybe a)
getCurrentRef ref = Nullable.toMaybe <$> runEffectFn1 getCurrentRef_ ref
