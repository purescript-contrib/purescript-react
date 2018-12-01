module React.Hook
  ( Hook

  , useState
  , useStateLazy
  , setState
  , modifyState
  , SetState

  , useEffect
  , effectInput
  , EffectInput

  , useContext
  , Context

  , useReducer
  , useReducerLazy
  , dispatch
  , Dispatch

  , useCallback
  , callbackInput
  , CallbackInput

  , useMemo
  , memoInput
  , MemoInput

  , useRef
  , getRef
  , setRef
  , Ref

  , useImperativeMethods
  , imperativeMethodsInput
  , ImperativeMethodsInput

  , useMutationEffect
  , mutationEffectInput
  , MutationEffectInput

  , useLayoutEffect
  , layoutEffectInput
  , LayoutEffectInput
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, mkFn2, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

import Unsafe.Coerce (unsafeCoerce)

useState
  :: forall a
   . a
  -> Hook (Tuple a (SetState a))
useState = runFn2 useState_ Tuple

useStateLazy
  :: forall a
   . (Unit -> a)
  -> Hook (Tuple a (SetState a))
useStateLazy = runFn2 useState_ Tuple

setState
  :: forall a
   . SetState a
  -> a
  -> Effect Unit
setState k = runEffectFn1 k'
  where
  k' :: EffectFn1 a Unit
  k' = unsafeCoerce k

modifyState
  :: forall a
   . SetState a
  -> (a -> a)
  -> Effect Unit
modifyState k = runEffectFn1 k'
  where
  k' :: EffectFn1 (a -> a) Unit
  k' = unsafeCoerce k

foreign import data SetState :: Type -> Type

foreign import useState_
  :: forall a b
   . Fn2 (b -> SetState b -> Tuple b (SetState b))
         a
         (Hook (Tuple b (SetState b)))

useEffect
  :: forall a
   . Effect (Effect a)
  -> Maybe (Array EffectInput)
  -> Hook Unit
useEffect k = runFn2 useEffect_ k <<< Nullable.toNullable

effectInput :: forall a. a -> EffectInput
effectInput = unsafeCoerce

foreign import data EffectInput :: Type

foreign import useEffect_
  :: forall a
   . Fn2 (Effect (Effect a))
         (Nullable (Array EffectInput))
         (Hook Unit)

useContext :: forall a. Context a -> Hook a
useContext = runFn1 useContext_

foreign import data Context :: Type -> Type

foreign import useContext_
  :: forall a
   . Fn1 (Context a)
         (Hook a)

useReducer
  :: forall a b
   . (a -> b -> a)
  -> a
  -> Hook (Tuple a (Dispatch a b))
useReducer = runFn3 useReducer_ Tuple <<< mkFn2

useReducerLazy
  :: forall a b
   . (a -> b -> a)
  -> a
  -> b
  -> Hook (Tuple a (Dispatch a b))
useReducerLazy = runFn4 useReducerLazy_ Tuple <<< mkFn2

dispatch
  :: forall a b
   . Dispatch a b
  -> a
  -> Effect Unit
dispatch k = runEffectFn1 k'
  where
  k' :: EffectFn1 a Unit
  k' = unsafeCoerce k

foreign import data Dispatch :: Type -> Type -> Type

foreign import useReducer_
  :: forall a b
   . Fn3 (a -> Dispatch a b -> Tuple a (Dispatch a b))
         (Fn2 a b a)
         a
         (Hook (Tuple a (Dispatch a b)))

foreign import useReducerLazy_
  :: forall a b
   . Fn4 (a -> Dispatch a b -> Tuple a (Dispatch a b))
         (Fn2 a b a)
         a
         b
         (Hook (Tuple a (Dispatch a b)))

useCallback
  :: forall a b
   . (a -> b)
  -> Maybe (Array CallbackInput)
  -> Hook (a -> b)
useCallback k = runFn2 useCallback_ k <<< Nullable.toNullable

callbackInput :: forall a. a -> CallbackInput
callbackInput = unsafeCoerce

foreign import data CallbackInput :: Type

foreign import useCallback_
  :: forall a b
   . Fn2 (a -> b)
         (Nullable (Array CallbackInput))
         (Hook (a -> b))

useMemo
  :: forall a b
   . (Unit -> a -> b)
  -> Maybe (Array MemoInput)
  -> Hook (a -> b)
useMemo k = runFn2 useMemo_ k <<< Nullable.toNullable

memoInput :: forall a. a -> MemoInput
memoInput = unsafeCoerce

foreign import data MemoInput :: Type

foreign import useMemo_
  :: forall a b
   . Fn2 (Unit -> a -> b)
         (Nullable (Array MemoInput))
         (Hook (a -> b))

useRef :: forall a. Maybe a -> Hook (Ref a)
useRef = runFn1 useRef_ <<< Nullable.toNullable

getRef :: forall a. Ref a -> Effect (Maybe a)
getRef r = Nullable.toMaybe <$> runEffectFn1 getRef_ r

setRef :: forall a. Ref a -> Maybe a -> Effect Unit
setRef r = runEffectFn2 setRef_ r <<< Nullable.toNullable

foreign import data Ref :: Type -> Type

foreign import useRef_
  :: forall a
   . Fn1 (Nullable a)
         (Hook (Ref a))

foreign import getRef_
  :: forall a
   . EffectFn1 (Ref a)
               (Nullable a)

foreign import setRef_
  :: forall a
   . EffectFn2 (Ref a)
               (Nullable a)
               Unit

useImperativeMethods
  :: forall r a
   . Ref a
  -> (Unit -> { | r })
  -> Maybe (Array ImperativeMethodsInput)
  -> Hook Unit
useImperativeMethods a k = runFn3 useImperativeMethods_ a k <<< Nullable.toNullable

imperativeMethodsInput :: forall a. a -> ImperativeMethodsInput
imperativeMethodsInput = unsafeCoerce

foreign import data ImperativeMethodsInput :: Type

foreign import useImperativeMethods_
  :: forall r a
   . Fn3 (Ref a)
         (Unit -> { | r })
         (Nullable (Array ImperativeMethodsInput))
         (Hook Unit)

useMutationEffect
  :: forall a
   . Effect (Effect a)
  -> Maybe (Array MutationEffectInput)
  -> Hook Unit
useMutationEffect k = runFn2 useMutationEffect_ k <<< Nullable.toNullable

mutationEffectInput :: forall a. a -> MutationEffectInput
mutationEffectInput = unsafeCoerce

foreign import data MutationEffectInput :: Type

foreign import useMutationEffect_
  :: forall a
   . Fn2 (Effect (Effect a))
         (Nullable (Array MutationEffectInput))
         (Hook Unit)

useLayoutEffect
  :: forall a
   . Effect (Effect a)
  -> Maybe (Array LayoutEffectInput)
  -> Hook Unit
useLayoutEffect k = runFn2 useLayoutEffect_ k <<< Nullable.toNullable

layoutEffectInput :: forall a. a -> LayoutEffectInput
layoutEffectInput = unsafeCoerce

foreign import data LayoutEffectInput :: Type

foreign import useLayoutEffect_
  :: forall a
   . Fn2 (Effect (Effect a))
         (Nullable (Array LayoutEffectInput))
         (Hook Unit)

foreign import data Hook :: Type -> Type

unHook :: forall a. Hook a -> a
unHook = unsafeCoerce

hook :: forall a. a -> Hook a
hook = unsafeCoerce

instance functorHook :: Functor Hook where
  map k = hook <<< k <<< unHook

instance applyHook :: Apply Hook where
  apply k fa = hook (unHook k (unHook fa))

instance applicativeHook :: Applicative Hook where
  pure = hook

instance bindHook :: Bind Hook where
  bind fa k = k (unHook fa)

instance monadHook :: Monad Hook
