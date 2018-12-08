module React.Context
  ( Context
  , createContext
  , getProvider
  , getConsumer
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable

import React.Types (ReactElement, ReactClass, Children)

createContext :: forall a. a -> Maybe (a -> a -> Number) -> Context a
createContext a = runFn2 createContext_ a <<< Nullable.toNullable <<< map mkFn2

foreign import data Context :: Type -> Type

foreign import getProvider :: forall a. Context a -> ReactClass { children :: Children, value :: a }

foreign import getConsumer :: forall a. Context a -> ReactClass { children :: a -> ReactElement }

foreign import createContext_
  :: forall a
   . Fn2 a
         (Nullable (Fn2 a a Number))
         (Context a)
