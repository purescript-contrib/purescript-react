module Main where

import Control.Monad.Eff
import Debug.Trace
import React
import React.DOM

foreign import interval
  "function interval(ms) { \
  \  return function(action) { \
  \    return function() { return setInterval(action, ms); } \
  \  } \
  \}"
  :: forall eff r. Number -> Eff (trace :: Trace) r -> Eff (eff) {}

helloInConsole e = do
  props <- getProps
  trace ("Hello, " ++ props.name ++ "!")

hello = mkUI spec do
  props <- getProps
  return $ h1 [
      className "Hello",
      onClick helloInConsole,
      style {background: "gray"}
    ] [
      text "Hello, ",
      text props.name
    ]

incrementCounter = do
  transformState \val -> val + 1

counter = mkUI spec {
    getInitialState = return 0,
    componentDidMount = do
      incrementCounterCallback <- deferred incrementCounter
      interval 1000 $ do
        incrementCounterCallback
        return {}
  } do
  val <- readState
  incrementCounterCallback <- deferred incrementCounter
  return $ div [className "Counter"] [
      p' [
        text "Current counter value is: ",
        text (show val),
        text " (it is incremented each second)"
      ],
      p' [
        text "Alternatively use this button to increment it manually:",
        incrementer {onIncrement: incrementCounterCallback}
      ]
    ]

incrementer = mkUI spec do
  props <- getProps
  return $ button [onClick \e -> props.onIncrement] [text "Click me to increment!"]

main = do
  let component = div' [hello {name: "World"}, counter {}]
  renderToBody component
