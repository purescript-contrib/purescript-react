-- | See the [Animation section](https://facebook.github.io/react/docs/animation.html) of the React documentation for details concerning usage/API.
-- | This API is byte-for-byte compatible.

-- | Note:  You must `npm install react-addons-css-transition-group` to use this module.

module React.Addons.Animation.Transition (reactCSSTransitionGroup) where

import Prelude ((<<<))

import React (ReactClass(), ReactElement(), createElementDynamic)
import React.DOM.Props (Props(), unsafeFromPropsArray)

reactCSSTransitionGroup :: Array Props -> Array ReactElement -> ReactElement
reactCSSTransitionGroup = createElementDynamic reactCSSTransitionGroupClass <<< unsafeFromPropsArray

foreign import reactCSSTransitionGroupClass :: forall props. ReactClass props
