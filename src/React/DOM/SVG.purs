module React.DOM.SVG where

import React
import React.DOM (mkDOM)
import React.DOM.Props (Props())

circle :: Array Props -> Array ReactElement -> ReactElement
circle = mkDOM "circle"

clipPath :: Array Props -> Array ReactElement -> ReactElement
clipPath = mkDOM "clipPath"

defs :: Array Props -> Array ReactElement -> ReactElement
defs = mkDOM "defs"

ellipse :: Array Props -> Array ReactElement -> ReactElement
ellipse = mkDOM "ellipse"

g :: Array Props -> Array ReactElement -> ReactElement
g = mkDOM "g"

line :: Array Props -> Array ReactElement -> ReactElement
line = mkDOM "line"

linearGradient :: Array Props -> Array ReactElement -> ReactElement
linearGradient = mkDOM "linearGradient"

mask :: Array Props -> Array ReactElement -> ReactElement
mask = mkDOM "mask"

path :: Array Props -> Array ReactElement -> ReactElement
path = mkDOM "path"

pattern :: Array Props -> Array ReactElement -> ReactElement
pattern = mkDOM "pattern"

polygon :: Array Props -> Array ReactElement -> ReactElement
polygon = mkDOM "polygon"

polyline :: Array Props -> Array ReactElement -> ReactElement
polyline = mkDOM "polyline"

radialGradient :: Array Props -> Array ReactElement -> ReactElement
radialGradient = mkDOM "radialGradient"

rect :: Array Props -> Array ReactElement -> ReactElement
rect = mkDOM "rect"

stop :: Array Props -> Array ReactElement -> ReactElement
stop = mkDOM "stop"

svg :: Array Props -> Array ReactElement -> ReactElement
svg = mkDOM "svg"

text :: Array Props -> Array ReactElement -> ReactElement
text = mkDOM "text"

tspan :: Array Props -> Array ReactElement -> ReactElement
tspan = mkDOM "tspan"
