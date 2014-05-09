{-# Language OverloadedStrings #-}
module Main where

import Clay

main = putCss $ do
    body ?
        padding (px 20) (px 20) (px 20) (px 20)
    form ? do
        width (px 250)
        ".search-input" ?
            display block
    ".red" ?
        color red
    thead ? do
        minWidth (px 400)
    td ? do
        minWidth (px 150)
    th ? do
        minWidth (px 150)
