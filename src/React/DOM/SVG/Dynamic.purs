module React.DOM.SVG.Dynamic where

import React (ReactElement())
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM.Props (Props())

circle :: Array Props -> Array ReactElement -> ReactElement
circle = mkDOM (IsDynamic true) "circle"

clipPath :: Array Props -> Array ReactElement -> ReactElement
clipPath = mkDOM (IsDynamic true) "clipPath"

defs :: Array Props -> Array ReactElement -> ReactElement
defs = mkDOM (IsDynamic true) "defs"

ellipse :: Array Props -> Array ReactElement -> ReactElement
ellipse = mkDOM (IsDynamic true) "ellipse"

g :: Array Props -> Array ReactElement -> ReactElement
g = mkDOM (IsDynamic true) "g"

line :: Array Props -> Array ReactElement -> ReactElement
line = mkDOM (IsDynamic true) "line"

linearGradient :: Array Props -> Array ReactElement -> ReactElement
linearGradient = mkDOM (IsDynamic true) "linearGradient"

mask :: Array Props -> Array ReactElement -> ReactElement
mask = mkDOM (IsDynamic true) "mask"

path :: Array Props -> Array ReactElement -> ReactElement
path = mkDOM (IsDynamic true) "path"

pattern :: Array Props -> Array ReactElement -> ReactElement
pattern = mkDOM (IsDynamic true) "pattern"

polygon :: Array Props -> Array ReactElement -> ReactElement
polygon = mkDOM (IsDynamic true) "polygon"

polyline :: Array Props -> Array ReactElement -> ReactElement
polyline = mkDOM (IsDynamic true) "polyline"

radialGradient :: Array Props -> Array ReactElement -> ReactElement
radialGradient = mkDOM (IsDynamic true) "radialGradient"

rect :: Array Props -> Array ReactElement -> ReactElement
rect = mkDOM (IsDynamic true) "rect"

stop :: Array Props -> Array ReactElement -> ReactElement
stop = mkDOM (IsDynamic true) "stop"

svg :: Array Props -> Array ReactElement -> ReactElement
svg = mkDOM (IsDynamic true) "svg"

text :: Array Props -> Array ReactElement -> ReactElement
text = mkDOM (IsDynamic true) "text"

tspan :: Array Props -> Array ReactElement -> ReactElement
tspan = mkDOM (IsDynamic true) "tspan"
