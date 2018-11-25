module React.Hooks
  ( useState
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

import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))

import Effect (Effect)

import Effect.Uncurried
  ( EffectFn1
  , EffectFn2
  , EffectFn3
  , EffectFn4
  , runEffectFn1
  , runEffectFn2
  , runEffectFn3
  , runEffectFn4
  )

import Unsafe.Coerce (unsafeCoerce)

useState
  :: forall a
   . a
  -> Effect (Tuple a (SetState a))
useState = runEffectFn2 useState_ Tuple

useStateLazy
  :: forall a
   . (Unit -> a)
  -> Effect (Tuple a (SetState a))
useStateLazy = runEffectFn2 useState_ Tuple

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
   . EffectFn2 (b -> SetState b -> Tuple b (SetState b))
               a
               (Tuple b (SetState b))

useEffect
  :: forall a
   . Effect (Effect a)
  -> Maybe (Array EffectInput)
  -> Effect Unit
useEffect k = runEffectFn2 useEffect_ k <<< Nullable.toNullable

effectInput :: forall a. a -> EffectInput
effectInput = unsafeCoerce

foreign import data EffectInput :: Type

foreign import useEffect_
  :: forall a
   . EffectFn2 (Effect (Effect a))
               (Nullable (Array EffectInput))
               Unit

useContext :: forall a. Context a -> Effect a
useContext = runEffectFn1 useContext_

foreign import data Context :: Type -> Type

foreign import useContext_
  :: forall a
   . EffectFn1 (Context a)
               a

useReducer
  :: forall a b
   . (a -> b -> a)
  -> a
  -> Effect (Tuple a (Dispatch a b))
useReducer = runEffectFn3 useReducer_ Tuple <<< mkFn2

useReducerLazy
  :: forall a b
   . (a -> b -> a)
  -> a
  -> b
  -> Effect (Tuple a (Dispatch a b))
useReducerLazy = runEffectFn4 useReducerLazy_ Tuple <<< mkFn2

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
   . EffectFn3 (a -> Dispatch a b -> Tuple a (Dispatch a b))
               (Fn2 a b a)
               a
               (Tuple a (Dispatch a b))

foreign import useReducerLazy_
  :: forall a b
   . EffectFn4 (a -> Dispatch a b -> Tuple a (Dispatch a b))
               (Fn2 a b a)
               a
               b
               (Tuple a (Dispatch a b))

useCallback
  :: forall a b
   . (a -> b)
  -> Maybe (Array CallbackInput)
  -> Effect (a -> b)
useCallback k = runEffectFn2 useCallback_ k <<< Nullable.toNullable

callbackInput :: forall a. a -> CallbackInput
callbackInput = unsafeCoerce

foreign import data CallbackInput :: Type

foreign import useCallback_
  :: forall a b
   . EffectFn2 (a -> b)
               (Nullable (Array CallbackInput))
               (a -> b)

useMemo
  :: forall a b
   . (Unit -> a -> b)
  -> Maybe (Array MemoInput)
  -> Effect (a -> b)
useMemo k = runEffectFn2 useMemo_ k <<< Nullable.toNullable

memoInput :: forall a. a -> MemoInput
memoInput = unsafeCoerce

foreign import data MemoInput :: Type

foreign import useMemo_
  :: forall a b
   . EffectFn2 (Unit -> a -> b)
               (Nullable (Array MemoInput))
               (a -> b)

useRef :: forall a. Maybe a -> Effect (Ref a)
useRef = runEffectFn1 useRef_ <<< Nullable.toNullable

getRef :: forall a. Ref a -> Maybe a
getRef r = Nullable.toMaybe r'.current
  where
  r' :: { current :: Nullable a }
  r' = unsafeCoerce r

setRef :: forall a. Ref a -> Maybe a -> Effect Unit
setRef r = runEffectFn2 setRef_ r <<< Nullable.toNullable

foreign import data Ref :: Type -> Type

foreign import useRef_
  :: forall a
   . EffectFn1 (Nullable a)
               (Ref a)

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
  -> Effect Unit
useImperativeMethods a k = runEffectFn3 useImperativeMethods_ a k <<< Nullable.toNullable

imperativeMethodsInput :: forall a. a -> ImperativeMethodsInput
imperativeMethodsInput = unsafeCoerce

foreign import data ImperativeMethodsInput :: Type

foreign import useImperativeMethods_
  :: forall r a
   . EffectFn3 (Ref a)
               (Unit -> { | r })
               (Nullable (Array ImperativeMethodsInput))
               Unit

useMutationEffect
  :: forall a
   . Effect (Effect a)
  -> Maybe (Array MutationEffectInput)
  -> Effect Unit
useMutationEffect k = runEffectFn2 useMutationEffect_ k <<< Nullable.toNullable

mutationEffectInput :: forall a. a -> MutationEffectInput
mutationEffectInput = unsafeCoerce

foreign import data MutationEffectInput :: Type

foreign import useMutationEffect_
  :: forall a
   . EffectFn2 (Effect (Effect a))
               (Nullable (Array MutationEffectInput))
               Unit

useLayoutEffect
  :: forall a
   . Effect (Effect a)
  -> Maybe (Array LayoutEffectInput)
  -> Effect Unit
useLayoutEffect k = runEffectFn2 useLayoutEffect_ k <<< Nullable.toNullable

layoutEffectInput :: forall a. a -> LayoutEffectInput
layoutEffectInput = unsafeCoerce

foreign import data LayoutEffectInput :: Type

foreign import useLayoutEffect_
  :: forall a
   . EffectFn2 (Effect (Effect a))
               (Nullable (Array LayoutEffectInput))
               Unit
