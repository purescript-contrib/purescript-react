module React.Hook
  ( Hook
  , HookInput
  , hookInput
  , useState
  , useStateLazy
  , setState
  , modifyState
  , SetState
  , useEffect
  , useContext
  , useReducer
  , useReducerLazy
  , dispatch
  , Dispatch
  , useCallback
  , useMemo
  , useRef
  , useImperativeMethods
  , useLayoutEffect
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, mkFn2, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

import Unsafe.Coerce (unsafeCoerce)

import React.Context (Context)
import React.Ref (Ref)

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
  -> Maybe (Array HookInput)
  -> Hook Unit
useEffect k = runFn2 useEffect_ k <<< Nullable.toNullable

foreign import useEffect_
  :: forall a
   . Fn2 (Effect (Effect a))
         (Nullable (Array HookInput))
         (Hook Unit)

useContext :: forall a. Context a -> Hook a
useContext = runFn1 useContext_

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
  -> b
  -> Effect Unit
dispatch k = runEffectFn1 k'
  where
  k' :: EffectFn1 b Unit
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
  -> Maybe (Array HookInput)
  -> Hook (a -> b)
useCallback k = runFn2 useCallback_ k <<< Nullable.toNullable

foreign import useCallback_
  :: forall a b
   . Fn2 (a -> b)
         (Nullable (Array HookInput))
         (Hook (a -> b))

useMemo
  :: forall a
   . (Unit -> a)
  -> Maybe (Array HookInput)
  -> Hook a
useMemo k = runFn2 useMemo_ k <<< Nullable.toNullable

foreign import useMemo_
  :: forall a
   . Fn2 (Unit -> a)
         (Nullable (Array HookInput))
         (Hook a)

useRef :: forall a. Maybe a -> Hook (Ref a)
useRef = runFn1 useRef_ <<< Nullable.toNullable

foreign import useRef_
  :: forall a
   . Fn1 (Nullable a)
         (Hook (Ref a))

useImperativeMethods
  :: forall a
   . Ref a
  -> (Unit -> a)
  -> Maybe (Array HookInput)
  -> Hook Unit
useImperativeMethods a k = runFn3 useImperativeMethods_ a k <<< Nullable.toNullable

foreign import useImperativeMethods_
  :: forall a
   . Fn3 (Ref a)
         (Unit -> a)
         (Nullable (Array HookInput))
         (Hook Unit)

useLayoutEffect
  :: forall a
   . Effect (Effect a)
  -> Maybe (Array HookInput)
  -> Hook Unit
useLayoutEffect k = runFn2 useLayoutEffect_ k <<< Nullable.toNullable

foreign import useLayoutEffect_
  :: forall a
   . Fn2 (Effect (Effect a))
         (Nullable (Array HookInput))
         (Hook Unit)

foreign import data Hook :: Type -> Type

instance functorHook :: Functor Hook where
  map k = hook <<< k <<< unHook

instance applyHook :: Apply Hook where
  apply k fa = hook (unHook k (unHook fa))

instance applicativeHook :: Applicative Hook where
  pure = hook

instance bindHook :: Bind Hook where
  bind fa k = k (unHook fa)

instance monadHook :: Monad Hook

unHook :: forall a. Hook a -> a
unHook = unsafeCoerce

hook :: forall a. a -> Hook a
hook = unsafeCoerce

foreign import data HookInput :: Type

hookInput :: forall a. a -> HookInput
hookInput = unsafeCoerce
