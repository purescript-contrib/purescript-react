module React.DOM.SVG where

import React (ReactElement)
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM.Props (Props)

circle :: Array Props -> Array ReactElement -> ReactElement
circle = mkDOM (IsDynamic false) "circle"

circle' :: Array ReactElement -> ReactElement
circle' = circle []

clipPath :: Array Props -> Array ReactElement -> ReactElement
clipPath = mkDOM (IsDynamic false) "clipPath"

clipPath' :: Array ReactElement -> ReactElement
clipPath' = clipPath []

defs :: Array Props -> Array ReactElement -> ReactElement
defs = mkDOM (IsDynamic false) "defs"

defs' :: Array ReactElement -> ReactElement
defs' = defs []

ellipse :: Array Props -> Array ReactElement -> ReactElement
ellipse = mkDOM (IsDynamic false) "ellipse"

ellipse' :: Array ReactElement -> ReactElement
ellipse' = ellipse []

foreignObject :: Array Props -> Array ReactElement -> ReactElement
foreignObject = mkDOM (IsDynamic false) "foreignObject"

g :: Array Props -> Array ReactElement -> ReactElement
g = mkDOM (IsDynamic false) "g"

g' :: Array ReactElement -> ReactElement
g' = g []

line :: Array Props -> Array ReactElement -> ReactElement
line = mkDOM (IsDynamic false) "line"

line' :: Array ReactElement -> ReactElement
line' = line []

linearGradient :: Array Props -> Array ReactElement -> ReactElement
linearGradient = mkDOM (IsDynamic false) "linearGradient"

linearGradient' :: Array ReactElement -> ReactElement
linearGradient' = linearGradient []

mask :: Array Props -> Array ReactElement -> ReactElement
mask = mkDOM (IsDynamic false) "mask"

mask' :: Array ReactElement -> ReactElement
mask' = mask []

path :: Array Props -> Array ReactElement -> ReactElement
path = mkDOM (IsDynamic false) "path"

path' :: Array ReactElement -> ReactElement
path' = path []

pattern :: Array Props -> Array ReactElement -> ReactElement
pattern = mkDOM (IsDynamic false) "pattern"

pattern' :: Array ReactElement -> ReactElement
pattern' = pattern []

polygon :: Array Props -> Array ReactElement -> ReactElement
polygon = mkDOM (IsDynamic false) "polygon"

polygon' :: Array ReactElement -> ReactElement
polygon' = polygon []

polyline :: Array Props -> Array ReactElement -> ReactElement
polyline = mkDOM (IsDynamic false) "polyline"

polyline' :: Array ReactElement -> ReactElement
polyline' = polyline []

radialGradient :: Array Props -> Array ReactElement -> ReactElement
radialGradient = mkDOM (IsDynamic false) "radialGradient"

radialGradient' :: Array ReactElement -> ReactElement
radialGradient' = radialGradient []

rect :: Array Props -> Array ReactElement -> ReactElement
rect = mkDOM (IsDynamic false) "rect"

rect' :: Array ReactElement -> ReactElement
rect' = rect []

stop :: Array Props -> Array ReactElement -> ReactElement
stop = mkDOM (IsDynamic false) "stop"

stop' :: Array ReactElement -> ReactElement
stop' = stop []

svg :: Array Props -> Array ReactElement -> ReactElement
svg = mkDOM (IsDynamic false) "svg"

svg' :: Array ReactElement -> ReactElement
svg' = svg []

text :: Array Props -> Array ReactElement -> ReactElement
text = mkDOM (IsDynamic false) "text"

text' :: Array ReactElement -> ReactElement
text' = text []

tspan :: Array Props -> Array ReactElement -> ReactElement
tspan = mkDOM (IsDynamic false) "tspan"

tspan' :: Array ReactElement -> ReactElement
tspan' = tspan []
