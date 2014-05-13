module Main where

import Control.Monad.Eff
import Debug.Trace
import React
import qualified React.DOM as DOM

foreign import interval
  "function interval(ms) { \
  \  return function(action) { \
  \    return function() { return setInterval(action, ms); } \
  \  } \
  \}"
  :: forall eff r. Number -> Eff (trace :: Trace) r -> Eff (eff) {}

helloInConsole = do
  props <- getProps
  trace ("Hello, " ++ props.name ++ "!")

hello = mkUI spec do
  props <- getProps
  return $ DOM.h1 {
      className: "Hello",
      onClick: handle helloInConsole
    } [
      DOM.text "Hello, ",
      DOM.text props.name
    ]

incrementCounter = do
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
  return $ DOM.p {
      className: "Counter",
      onClick: handle incrementCounter
    } [
      DOM.text (show val),
      DOM.text " Click me to increment!"
    ]

main = do
  let component = DOM.div {} [hello {name: "World"}, counter {}]
  renderToBody component
