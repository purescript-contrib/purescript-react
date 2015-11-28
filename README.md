purescript-react
================

[![Maintainer: paf31](https://img.shields.io/badge/maintainer-paf31-lightgrey.svg)](http://github.com/paf31) ![React: 0.13.3](https://img.shields.io/badge/react-0.13.3-lightgrey.svg)

Low-level React Bindings for PureScript.

For a more high-level set of bindings, you might like to look at `purescript-thermite`.

- [Module Documentation](docs/)

## Dynamic children

There are two ways that child elements can be passed to components:
1. The first way is to spread the child element array when passing
   them to React's `createElement` function.
   The [React.DOM](docs/React/DOM.md) and [React.DOM.SVG](docs/React/DOM/SVG.md)
   spread the child element array.
2. The second way is to pass the child element array to `createElement`
   without spreading. This is useful when dealing with
   [dynamic children](https://facebook.github.io/react/docs/multiple-components.html#dynamic-children).
   In this case a `key` property should be assigned direclty to each child.
   The [React.DOM.Dynamic](docs/React/DOM/Dynamic.md) and
   [React.DOM.SVG.Dynamic](docs/React/DOM/SVG/Dynamic.md) pass the child
   element array without spreading.

Note that this module provides functions `createElement` and
`createElementDynamic` to handle the two cases above for component
classes.

## Building

The library and example can be built with Pulp as follows:

    pulp dep update
    pulp build

    pulp test -r cat > example/index.js
    open example/index.html

## Example

```purescript
module Main where

import Prelude

import Control.Monad.Eff

import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)

import DOM.Node.Types (Element())

import React

import qualified React.DOM as D
import qualified React.DOM.Props as P

incrementCounter ctx e = do
  val <- readState ctx
  writeState ctx (val + 1)

counter = createClass $ spec 0 \ctx -> do
  val <- readState ctx
  return $ D.p [ P.className "Counter"
               , P.onClick (incrementCounter ctx)
               ]
               [ D.text (show val)
               , D.text " Click me to increment!"
               ]

main = container >>= render ui
  where
  ui :: ReactElement
  ui = D.div [] [ createFactory counter {} ]

  container :: forall eff. Eff (dom :: DOM | eff) Element
  container = do
    win <- window
    doc <- document win
    elm <- fromJust <$> toMaybe <$> body doc
    return $ htmlElementToElement elm
```
