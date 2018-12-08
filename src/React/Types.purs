module React.Types
  ( ReactClass
  , ReactElement
  , class IsReactElement
  , toElement
  , Children
  , childrenToArray
  , childrenCount
  ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

-- | React class for components.
foreign import data ReactClass :: Type -> Type

-- | A virtual DOM node, or component.
foreign import data ReactElement :: Type

instance semigroupReactElement :: Semigroup ReactElement where
  append a b = toElement [ a, b ]

instance monoidReactElement :: Monoid ReactElement where
  mempty = toElement ([] :: Array ReactElement)

class IsReactElement a where
  toElement :: a -> ReactElement

instance isReactElementString :: IsReactElement String where
  toElement = unsafeCoerce

instance isReactElementNumber :: IsReactElement Number where
  toElement = unsafeCoerce

instance isReactElementInt :: IsReactElement Int where
  toElement = unsafeCoerce

instance isReactElementChildren :: IsReactElement Children where
  toElement = unsafeCoerce

instance isReactElementReactElement :: IsReactElement ReactElement where
  toElement = identity

instance isReactElementArray :: IsReactElement (Array ReactElement) where
  toElement = toElementArray

foreign import toElementArray :: Array ReactElement -> ReactElement

-- | Internal representation for the children elements passed to a component
foreign import data Children :: Type

-- | Internal conversion function from children elements to an array of React elements
foreign import childrenToArray :: Children -> Array ReactElement

-- | Returns the number of children.
foreign import childrenCount :: Children -> Int
