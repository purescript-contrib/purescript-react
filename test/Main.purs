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

hello = mkUI (spec unit) \ctx -> do
  props <- getProps ctx
  return $ h1 [ className "Hello"
              , style { background: "lightgray" }
              ] 
              [ text "Hello, "
              , text props.name
              ]

counter = mkUI counterSpec \ctx -> do
  val <- readState ctx
  return $ button [ className "Counter"
                  , onClick \_ -> do
                      val <- readState ctx
                      writeState ctx (val + 1)
                  ]
                  [ text (show val)
                  , text " Click me to increment!"
                  ]
  where
  counterSpec = (spec 0)
    { componentDidMount = \ctx ->
        interval 1000 $ do
          val <- readState ctx
          print val
    }

main = do
  let component = div' [ hello { name: "World" }, counter unit ]
  renderToBody component
