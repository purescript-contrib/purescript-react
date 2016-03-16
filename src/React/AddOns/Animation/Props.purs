-- | See the [Animation section](https://facebook.github.io/react/docs/animation.html) of the React documentation for details concerning usage/API.
-- | This API is byte-for-byte compatible.

-- | Note:  You must `npm install react-addons-css-transition-group` to use this module.

module React.Addons.Animation.Props where

import React.DOM.Props

transitionName :: String -> Props
transitionName = unsafeMkProps "transitionName"

type TransitionNames =
  { enter        :: String
  , enterActive  :: String
  , leave        :: String
  , leaveActive  :: String
  , appearActive :: String
  }

transitionName' :: TransitionNames -> Props
transitionName' = unsafeUnfoldProps "transitionName"

transitionAppear :: Boolean -> Props
transitionAppear = unsafeMkProps "transitionAppear"

transitionEnter :: Boolean -> Props
transitionEnter = unsafeMkProps "transitionEnter"

transitionLeave :: Boolean -> Props
transitionLeave = unsafeMkProps "transitionLeave"

transitionEnterTimeout :: Int -> Props
transitionEnterTimeout = unsafeMkProps "transitionEnterTimeout"

transitionLeaveTimeout :: Int -> Props
transitionLeaveTimeout = unsafeMkProps "transitionLeaveTimeout"

component :: String -> Props
component = unsafeMkProps "component"
