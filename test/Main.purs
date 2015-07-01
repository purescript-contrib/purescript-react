module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import React

import qualified React.DOM as D
import qualified React.DOM.Props as P

foreign import interval :: forall eff a. 
                             Int -> 
                             Eff (console :: CONSOLE | eff) a ->
                             Eff (console :: CONSOLE | eff) Unit

hello = mkUI (spec unit) \ctx -> do
  props <- getProps ctx
  return $ D.h1 [ P.className "Hello"
                , P.style { background: "lightgray" }
                ] 
                [ D.text "Hello, "
                , D.text props.name
                ]

counter = mkUI counterSpec \ctx -> do
  val <- readState ctx
  return $ D.button [ P.className "Counter"
                    , P.onClick \_ -> do
                        val <- readState ctx
                        writeState ctx (val + 1)
                    ]
                    [ D.text (show val)
                    , D.text " Click me to increment!"
                    ]
  where
  counterSpec = (spec 0)
    { componentDidMount = \ctx ->
        interval 1000 $ do
          val <- readState ctx
          print val
    }

main = do
  let component = D.div' [ hello { name: "World" }, counter unit ]
  renderToBody component
