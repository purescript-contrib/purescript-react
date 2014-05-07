module Main where

import Debug.Trace
import React
import qualified React.DOM as DOM

helloInConsole = do
  props <- getProps
  trace ("Hello, " ++ props.name ++ "!")

hello = mkUI do
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

counter = mkStatefulUI 0 do
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
