purescript-react
================

[![Maintainer: paf31](https://img.shields.io/badge/maintainer-paf31-lightgrey.svg)](http://github.com/paf31) ![React: 0.12.2](https://img.shields.io/badge/react-0.12.2-lightgrey.svg)

Low-level React Bindings for PureScript.

For a more high-level set of bindings, you might like to look at `purescript-thermite`.

- [Module Documentation](docs/)

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

import React

import qualified React.DOM as D
import qualified React.DOM.Props as P

incrementCounter ctx e = do
  val <- readState ctx
  writeState ctx (val + 1)

counter = mkUI (spec 0) \ctx -> do
  val <- readState ctx
  return $ D.p [ P.className "Counter"
               , P.onClick (incrementCounter ctx)
               ] 
               [ D.text (show val)
               , D.text " Click me to increment!"
               ]

main = do
  let component = D.div [] [ counter {} ]
  renderToBody component
```
