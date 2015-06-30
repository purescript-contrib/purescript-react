module Test.Main where

import Prelude hiding (sub, div)

import Control.Monad.Eff
import Control.Monad.Eff.Console

import React
import React.DOM

foreign import interval :: forall eff a. 
                             Int -> 
                             Eff (console :: CONSOLE | eff) a ->
                             Eff (console :: CONSOLE | eff) Unit

helloInConsole e = do
  props <- getProps
  log ("Hello, " ++ props.name ++ "!")

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
