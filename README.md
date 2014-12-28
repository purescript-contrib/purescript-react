purescript-react
================

React Bindings for PureScript.

**WARNING:** This is alpha quaility software and you need to use [nightly build
of React][nightly].

```haskell
module Main where

import React
import React.DOM

hello = mkUI spec do
  props <- getProps
  return $ h1 [
      className "Hello"
    ] [
      text "Hello, ",
      text props.name
    ]

incrementCounter = do
  val <- readState
  writeState (val + 1)

counter = mkUI spec { getInitialState = return 0 } do
  val <- readState
  return $ p [
      className "Counter",
      onClick incrementCounter
    ] [
      text (show val),
      text " Click me to increment!"
    ]

main = do
  let component = div [] [hello {name: "World"}, counter {}]
  renderToBody component
```

[nightly]: http://react.zpao.com/builds/master/latest/react.js
