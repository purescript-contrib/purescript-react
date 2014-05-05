module Main where

import React

hello = mkUI \props ->
  div {className: "Hello"} [text "Hello, ", text props.name]

incrementCounter = do
  val <- readState
  writeState (val + 1)

counter = mkStatefulUI 0 \props -> do
  val <- readState
  return (div {className: "Counter", onClick: handle incrementCounter} [text (show val)])

main = do
  let component = div {} [counter {}, hello {name: "World"}, counter {}]
  renderToBody component
