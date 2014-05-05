module Main where

import React
import qualified React.DOM as DOM

hello = mkUI \props ->
  DOM.h1 {className: "Hello"} [
    DOM.text "Hello, ",
    DOM.text props.name
  ]

incrementCounter = do
  val <- readState
  writeState (val + 1)

counter = mkStatefulUI 0 \props -> do
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
