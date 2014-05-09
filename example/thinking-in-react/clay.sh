#!/bin/sh

# This probably won't work in general.
ghc -package-db --make .cabal-sandbox/x86_64-linux-ghc-7.8.2-packages.conf.d style.hs
./style > style.css
rm style
