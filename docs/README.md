# React Documentation

This directory contains documentation for `react`. If you are interested in contributing new documentation, please read the [contributor guidelines](../CONTRIBUTING.md) and [What Nobody Tells You About Documentation](https://documentation.divio.com) for help getting started.

## Frequently Asked Questions

### How to use JavaScript components?

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

import Effect.Uncurried (mkEffectFn1)

import React as React
import React.SyntheticEvent as Event

import Clock as Clock

clock :: React.ReactElement
clock =
  React.createElement Clock.clockComponent
    { format: "HH:mm:ss"
    , className: "test-class-name"
    , onTick: mkEffectFn1 $ \event -> do
        Event.preventDefault event
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

import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)

import React (ReactClass, ReactElement, Children, createElement)
import React.SyntheticEvent (SyntheticEvent)
import React.DOM.Props (Props, unsafeFromPropsArray, unsafeMkProps)

clockComponent :: Array Props -> Array ReactElement -> ReactElement
clockComponent props children = createElement clockComponent_ (unsafeFromPropsArray props :: {}) children

format :: String -> Props
format = unsafeMkProps "format"

onTick :: (SyntheticEvent -> Effect Unit) -> Props
onTick k = unsafeMkProps "onTick" (mkEffectFn1 k)

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
import React.SyntheticEvent as Event
import React.DOM.Props as Props

import Clock as Clock

clock :: React.ReactElement
clock =
  Clock.clockComponent
    [ Clock.format "HH:mm:ss"
    , Clock.onTick $ \event -> do
        Event.preventDefault event
        -- etc.
        pure unit
    , Props.className "test-class-name"
    , Props.style
        { fontWeight: "bold"
        , color: "blue"
        }
    -- additional Props.*
    ] []
```

### Components with type class constraints re-mount on every render?

Consider the following example where an ordered list component is
defined for any item of type `a`, where `a` is constrained to have an
`Ord` type class instance.

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

-- This would be defined where the type parameter `a` is known.

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
  return React.component(React.reactComponentSpec()())("OrderedList")(component);
};
```

Above, the component creation is wrapped by the function with the
`dictOrd` parameter. This means that a new component is being created on
each render of the component using `orderedList`. This may not be ideal
in all cases; e.g., if `orderedList` had needed to store state.

To avoid `orderedList` from being recreated each time, a function can be
defined that specifies the type parameter with the type class contraint.
If the component using the ordered list knows that the items are of type
`Int`, the component can define `orderedListInt` as shown above, and use
it to render the ordered list instead of `orderedList`.


### Understanding `Children`

In React, we see the `children` prop type from time to time, especially
when using `createElement`. This is an opaque data type, in which we can
coerce into an `Array`, but we cannot create. Usually, when you see a
`ReactClass` that features a `children :: Children` prop type, this
means that the component itself expects children to be supplied as an
argument to `createElement`, in the form of an `Array ReactElement`.

However, in some circumstances (like a `ContextConsumer`), the `children`
prop type might look different, like `children :: a -> ReactElement`.
In this case, it would be better to use `createLeafElement`, to supply
the children _directly through the props_, rather than as a separate
argument.

This also means that any leaf-like components should _not_ define a
`children :: Children` prop - this prop should be treated as the
_expectation_ of a children argument. In the clock example above, a
more proper specification might look like the following:

```purescript
module Clock (clockComponent) where

import React (ReactClass, SyntheticEventHandler)
import React.SyntheticEvent (SyntheticEvent)

foreign import clockComponent
  :: ReactClass
      { format :: String
      , className :: String
      , onTick :: SyntheticEventHandler SyntheticEvent
      }
```

```purescript
module Component where

import Prelude

import Effect.Uncurried (mkEffectFn1)

import React as React
import React.SyntheticEvent as Event

import Clock as Clock

clock :: React.ReactElement
clock =
  React.createLeafElement Clock.clockComponent
    { format: "HH:mm:ss"
    , className: "test-class-name"
    , onTick: mkEffectFn1 $ \event -> do
        Event.preventDefault event
        -- etc.
        pure unit
    }
```