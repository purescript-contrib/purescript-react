module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)

import DOM.Node.Types (Element())

import React

import qualified React.DOM as D
import qualified React.DOM.Props as P

import Test.Container (container)

foreign import interval :: forall eff a.
                             Int ->
                             Eff eff a ->
                             Eff eff Unit

hello = mkUI $ spec unit \ctx -> do
  props <- getProps ctx
  return $ D.h1 [ P.className "Hello"
                , P.style { background: "lightgray" }
                ]
                [ D.text "Hello, "
                , D.text props.name
                ]

counter = mkUI counterSpec
  where
  counterSpec = (spec 0 render)
    { componentDidMount = \ctx ->
        interval 1000 $ do
          val <- readState ctx
          print val
    }

  render ctx = do
    val <- readState ctx
    return $ D.button [ P.className "Counter"
                      , P.onClick \_ -> do
                          val <- readState ctx
                          writeState ctx (val + 1)
                      ]
                      [ D.text (show val)
                      , D.text " Click me to increment!"
                      ]

main = body' >>= render ui
  where
  ui :: UI
  ui = D.div' [ createFactory hello { name: "World" }
              , createFactory counter unit
              , createElement container unit
                              [ D.p [] [ D.text  "This is line one" ]
                              , D.p [] [ D.text "This is line two" ]
                              ]
              ]

  body' :: forall eff. Eff (dom :: DOM | eff) Element
  body' = do
    win <- window
    doc <- document win
    elm <- fromJust <$> toMaybe <$> body doc
    return $ htmlElementToElement elm

