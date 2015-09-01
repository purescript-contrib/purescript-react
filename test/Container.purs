module Test.Container where

import Prelude

import React

import qualified React.DOM as D
import qualified React.DOM.Props as P

container = createClass $ spec unit \ctx -> do
  children <- getChildren ctx

  let ui = D.div [ P.style { borderColor: "red"
                           , borderWidth: 2
                           , padding: 10
                           }
                 ] children

  return ui
