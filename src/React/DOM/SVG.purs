module React.DOM.SVG where
    
import React
import React.DOM (mkDOM)
import React.DOM.Props (Props())
    
circle :: Array Props -> Array UI -> UI
circle = mkDOM "circle"

clipPath :: Array Props -> Array UI -> UI
clipPath = mkDOM "clipPath"

defs :: Array Props -> Array UI -> UI
defs = mkDOM "defs"

ellipse :: Array Props -> Array UI -> UI
ellipse = mkDOM "ellipse"

g :: Array Props -> Array UI -> UI
g = mkDOM "g"

line :: Array Props -> Array UI -> UI
line = mkDOM "line"

linearGradient :: Array Props -> Array UI -> UI
linearGradient = mkDOM "linearGradient"

mask :: Array Props -> Array UI -> UI
mask = mkDOM "mask"

path :: Array Props -> Array UI -> UI
path = mkDOM "path"

pattern :: Array Props -> Array UI -> UI
pattern = mkDOM "pattern"

polygon :: Array Props -> Array UI -> UI
polygon = mkDOM "polygon"

polyline :: Array Props -> Array UI -> UI
polyline = mkDOM "polyline"

radialGradient :: Array Props -> Array UI -> UI
radialGradient = mkDOM "radialGradient"

rect :: Array Props -> Array UI -> UI
rect = mkDOM "rect"

stop :: Array Props -> Array UI -> UI
stop = mkDOM "stop"

svg :: Array Props -> Array UI -> UI
svg = mkDOM "svg"

text :: Array Props -> Array UI -> UI
text = mkDOM "text"

tspan :: Array Props -> Array UI -> UI
tspan = mkDOM "tspan"