module Main where

import Control.Monad.Eff

import Data.Function

import Debug.Trace

import DOM

import React
import React.Types

import qualified React.DOM as D

type HelloState = {}
type HelloProps = {name :: String}
type CounterState = {val :: Number}
type CounterProps = {}

helloInConsole :: forall eff fields state
               .  ReactThis fields HelloProps HelloState
               -> ReactFormEvent
               -> Eff (trace :: Trace | eff) Unit
helloInConsole this _ = trace $ "Hello, " ++ this.props.name ++ "!"

hello :: ComponentClass HelloProps HelloState
hello = createClass spec
  { render = \this -> pure $ D.h1
    { className: "Hello"
    , onClick: eventHandler this helloInConsole
    , style: {background: "gray"}
    }
    [D.rawText "Hello, ", D.rawText this.props.name]
  }

incrementCounter :: forall eff fields
                 .  ReactThis fields CounterProps CounterState
                 -> ReactFormEvent
                 -> Eff eff Unit
incrementCounter this _ = pure $ this.replaceState {val: this.state.val + 1}

counter :: ComponentClass CounterProps CounterState
counter = createClass spec
  { render = \this -> pure $ D.p
    { className: "Counter"
    , onClick: eventHandler this incrementCounter
    }
    [ D.rawText $ show this.state.val
    , D.rawText " Click me to increment!"
    ]
  , getInitialState = \_ -> pure {val: 0}
  , componentDidMount = \this ->
    setInterval 1000 $ print this.state.val
  }

main :: Eff (react :: React, dom :: DOM) Component
main = let component = D.div {} [hello {name: "World"} [], counter {} []] in
  renderComponent component document.body

foreign import setInterval
  "function setInterval(ms) {\
  \  return function(action) {\
  \    return function() {\
  \      return setInterval(action(), ms);\
  \    }\
  \  }\
  \}"
  :: forall eff r. Number -> Eff (trace :: Trace) r -> Eff eff Unit
