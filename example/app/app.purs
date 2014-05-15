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
  return $ h1 [className "Hello", onClick helloInConsole] [
      text "Hello, ",
      text props.name
    ]

incrementCounter e = do
  val <- readState
  writeState (val + 1)

counter = mkUI spec {
    getInitialState = return 0,
    componentDidMount = do
      self <- getSelf
      interval 1000 $ runUI self do
        val <- readState
        print val
  } do
  val <- readState
  return $ p [className "Counter", onClick incrementCounter] [
      text (show val),
      text " Click me to increment!"
    ]

main = do
  let component = div' [hello {name: "World"}, counter {}]
  renderToBody component
