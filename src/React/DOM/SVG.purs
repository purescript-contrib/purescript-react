module React.DOM.SVG where

import React (ReactElement())
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM.Props (Props())

circle :: Array Props -> Array ReactElement -> ReactElement
circle = mkDOM (IsDynamic false) "circle"

clipPath :: Array Props -> Array ReactElement -> ReactElement
clipPath = mkDOM (IsDynamic false) "clipPath"

defs :: Array Props -> Array ReactElement -> ReactElement
defs = mkDOM (IsDynamic false) "defs"

ellipse :: Array Props -> Array ReactElement -> ReactElement
ellipse = mkDOM (IsDynamic false) "ellipse"

g :: Array Props -> Array ReactElement -> ReactElement
g = mkDOM (IsDynamic false) "g"

line :: Array Props -> Array ReactElement -> ReactElement
line = mkDOM (IsDynamic false) "line"

linearGradient :: Array Props -> Array ReactElement -> ReactElement
linearGradient = mkDOM (IsDynamic false) "linearGradient"

mask :: Array Props -> Array ReactElement -> ReactElement
mask = mkDOM (IsDynamic false) "mask"

path :: Array Props -> Array ReactElement -> ReactElement
path = mkDOM (IsDynamic false) "path"

pattern :: Array Props -> Array ReactElement -> ReactElement
pattern = mkDOM (IsDynamic false) "pattern"

polygon :: Array Props -> Array ReactElement -> ReactElement
polygon = mkDOM (IsDynamic false) "polygon"

polyline :: Array Props -> Array ReactElement -> ReactElement
polyline = mkDOM (IsDynamic false) "polyline"

radialGradient :: Array Props -> Array ReactElement -> ReactElement
radialGradient = mkDOM (IsDynamic false) "radialGradient"

rect :: Array Props -> Array ReactElement -> ReactElement
rect = mkDOM (IsDynamic false) "rect"

stop :: Array Props -> Array ReactElement -> ReactElement
stop = mkDOM (IsDynamic false) "stop"

svg :: Array Props -> Array ReactElement -> ReactElement
svg = mkDOM (IsDynamic false) "svg"

text :: Array Props -> Array ReactElement -> ReactElement
text = mkDOM (IsDynamic false) "text"

tspan :: Array Props -> Array ReactElement -> ReactElement
tspan = mkDOM (IsDynamic false) "tspan"
