# purescript-react

[![Maintainer: ethul](https://img.shields.io/badge/maintainer-ethul-lightgrey.svg)](http://github.com/ethul)
[![Maintainer: paf31](https://img.shields.io/badge/maintainer-paf31-lightgrey.svg)](http://github.com/paf31)
![React: 16](https://img.shields.io/badge/react-16-lightgrey.svg)

Low-level React Bindings for PureScript.

For a more high-level set of bindings, you might like to look at `purescript-thermite`.

- [Module Documentation](https://pursuit.purescript.org/packages/purescript-react/)

```
bower install purescript-react
```

This library requires the `react` module. This dependency may be satisfied by installing the NPM [react package](https://www.npmjs.com/package/react).

```
npm install react
```

## Related Modules

- [React DOM](https://github.com/purescript-contrib/purescript-react-dom)
- [CSS Transition Group Addon](https://github.com/purescript-contrib/purescript-react-addons-css-transition-group)

## Example

Please refer to [purescript-react-example](https://github.com/ethul/purescript-react-example)

## Troubleshooting

#### How to use JavaScript components?

To use a React component that is published as a JavaScript module, one
can leverage PureScript's FFI to define a type for the component and its
props. Consider the following example.

```purescript
module Clock (clockComponent) where

import React (ReactClass, SyntheticEventHandler, Children)
import React.SyntheticEvent (SyntheticEvent)

foreign import clockComponent
  :: ReactClass
      { children :: Children
      , format :: String
      , className :: String
      , onTick :: SyntheticEventHandler SyntheticEvent
      }
```

Rendering the `clockComponent` can be done as follows.

```purescript
module Component where

import Prelude

import React as React

import Clock as Clock

clock :: React.ReactElement
clock =
  React.createElement Clock.clockComponent
    { format: "HH:mm:ss"
    , className: "test-class-name"
    , onTick: React.handle $ \event -> do
        React.preventDefault event
        -- etc.
        pure unit
    } []
```

A consideration when defining a type for an external component is that
some components pass their props through to a DOM element. In a case
such as this, it can be helpful to leverage the props defined in the
`React.DOM.Props` module. One way to accomplish this is to define the
external component as follows.

```purescript
module Clock
  ( clockComponent
  , format
  , onTick
  ) where

import Prelude

import React (ReactClass, ReactElement, SyntheticEventHandlerContext, Children, createElement, handle)
import React.SyntheticEvent (SyntheticEvent)
import React.DOM.Props (Props, unsafeFromPropsArray, unsafeMkProps)

clockComponent :: Array Props -> Array ReactElement -> ReactElement
clockComponent props children = createElement clockComponent_ (unsafeFromPropsArray props :: {}) children

format :: String -> Props
format = unsafeMkProps "format"

onTick :: forall eff props state. (SyntheticEvent -> SyntheticEventHandlerContext eff props state Unit) -> Props
onTick k = unsafeMkProps "onTick" (handle k)

foreign import clockComponent_
  :: ReactClass
      { children :: Children
      }
```

Rendering the `clockComponent` can be done as follows.

```purescript
module Component where

import Prelude

import React as React
import React.DOM.Props as Props

import Clock as Clock

clock :: React.ReactElement
clock =
  Clock.clockComponent
    [ Clock.format "HH:mm:ss"
    , Clock.onTick $ \event -> do
        React.preventDefault event
        -- etc.
        pure unit
    , Props.className "test-class-name"
    , Props.style
        { fontWeight: "bold"
        , color: "blue"
        }
    -- additional Props.*
    ]
    [ ]
```

#### Components with type class constraints re-mount on every render?

Consider the following example where an ordered list component is
defined for any item of type `a`, where `a` is constrained to have a
type class instance `Ord`.

```purescript
module OrderedList where

import Prelude

import Data.Array (sort)

import React as React
import React.DOM as DOM
import Debug.Trace as Trace

type OrderedListProps a
  = { items :: Array a
    , renderItem :: a -> React.ReactElement
    }

orderedList :: forall a. Ord a => React.ReactClass (OrderedListProps a)
orderedList = React.component "OrderedList" component
  where
  component this =
    pure { state: {}
         , componentDidMount: do
             _ <- pure $ Trace.spy "OrderedList.componentDidMount"
             pure unit
         , render: render <$> React.getProps this
         }
    where
    render
      { items
      , renderItem
      } =
      DOM.ol [ ] $
        renderItem' <$> sort items
      where
      renderItem' a =
        DOM.li
          [ ]
          [ renderItem a ]

-- This may be defined elsewhere where the type parameter `a` is known.
-- In this case it is defined to be `Int`.

orderedListInt :: React.ReactClass (OrderedListProps Int)
orderedListInt = orderedList
```

If the component `orderedList` above were to be rendered, the debugging
statement `OrderedList.componentDidMount` is printed to the console each
time the parent component is rendered. The reason for this is due to how
the `orderedList` component is compiled to JavaScript.

```javascript
var orderedList = function (dictOrd) {
  var component = function ($$this) {
    // ...
  };
  return React.component()("OrderedList")(component);
};
```

Above, the component creation is wrapped by the function with the
`dictOrd` parameter. This means that a new component is being recreated
on each render of the component using `orderedList`. This may not be
ideal in all cases; e.g., if `orderedList` had needed to store state.

To avoid `orderedList` from being recreated each time, a function can be
defined that specifies the type parameter with the type class contraint.
If the component using the ordered list knows that the elements are of
type `Int`, the component can define `orderedListInt` as shown above,
and use it to render the ordered list instead of `orderedList`.
