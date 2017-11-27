module React.DOM.Props where

import Data.Foreign (Foreign)
import Data.Nullable (Nullable)
import React (Event, EventHandlerContext, KeyboardEvent, MouseEvent, handle)

foreign import data Props :: Type

foreign import unsafeMkProps :: forall val. String -> val -> Props

foreign import unsafeUnfoldProps :: forall vals. String -> { | vals } -> Props

foreign import unsafePrefixProps :: forall vals. String -> { | vals } -> Props

foreign import unsafeFromPropsArray :: forall props. Array Props -> props

aria :: forall ariaAttrs. { | ariaAttrs } -> Props
aria = unsafePrefixProps "aria-"

_data :: forall dataAttrs. { | dataAttrs } -> Props
_data = unsafePrefixProps "data-"

style :: forall style. { | style } -> Props
style = unsafeUnfoldProps "style"

dangerouslySetInnerHTML :: { __html :: String } -> Props
dangerouslySetInnerHTML = unsafeMkProps "dangerouslySetInnerHTML"

accept :: String -> Props
accept = unsafeMkProps "accept"

acceptCharset :: String -> Props
acceptCharset = unsafeMkProps "acceptCharset"

accessKey :: String -> Props
accessKey = unsafeMkProps "accessKey"

action :: String -> Props
action = unsafeMkProps "action"

allowFullScreen :: Boolean -> Props
allowFullScreen = unsafeMkProps "allowFullScreen"

allowTransparency :: Boolean -> Props
allowTransparency = unsafeMkProps "allowTransparency"

alt :: String -> Props
alt = unsafeMkProps "alt"

async :: Boolean -> Props
async = unsafeMkProps "async"

autoComplete :: String -> Props
autoComplete = unsafeMkProps "autoComplete"

autoFocus :: Boolean -> Props
autoFocus = unsafeMkProps "autoFocus"

autoPlay :: Boolean -> Props
autoPlay = unsafeMkProps "autoPlay"

capture :: Boolean -> Props
capture = unsafeMkProps "capture"

cellPadding :: String -> Props
cellPadding = unsafeMkProps "cellPadding"

cellSpacing :: String -> Props
cellSpacing = unsafeMkProps "cellSpacing"

charSet :: String -> Props
charSet = unsafeMkProps "charSet"

challenge :: String -> Props
challenge = unsafeMkProps "checked"

checked :: Boolean -> Props
checked = unsafeMkProps "checked"

cite :: String -> Props
cite = unsafeMkProps "cite"

classID :: String -> Props
classID = unsafeMkProps "classID"

className :: String -> Props
className = unsafeMkProps "className"

cols :: Int -> Props
cols = unsafeMkProps "cols"

colSpan :: Int -> Props
colSpan = unsafeMkProps "colSpan"

content :: String -> Props
content = unsafeMkProps "content"

contentEditable :: Boolean -> Props
contentEditable = unsafeMkProps "contentEditable"

contextMenu :: String -> Props
contextMenu = unsafeMkProps "contextMenu"

controls :: Boolean -> Props
controls = unsafeMkProps "controls"

coords :: String -> Props
coords = unsafeMkProps "coords"

crossOrigin :: String -> Props
crossOrigin = unsafeMkProps "crossOrigin"

dateTime :: String -> Props
dateTime = unsafeMkProps "dateTime"

default :: Boolean -> Props
default = unsafeMkProps "default"

defaultChecked :: Boolean -> Props
defaultChecked = unsafeMkProps "defaultChecked"

defaultValue :: String -> Props
defaultValue = unsafeMkProps "defaultValue"

defer :: Boolean -> Props
defer = unsafeMkProps "defer"

dir :: String -> Props
dir = unsafeMkProps "dir"

disabled :: Boolean -> Props
disabled = unsafeMkProps "disabled"

download :: String -> Props
download = unsafeMkProps "download"

draggable :: Boolean -> Props
draggable = unsafeMkProps "draggable"

encType :: String -> Props
encType = unsafeMkProps "encType"

form :: String -> Props
form = unsafeMkProps "form"

formAction :: String -> Props
formAction = unsafeMkProps "formAction"

formEncType :: String -> Props
formEncType = unsafeMkProps "formEncType"

formMethod :: String -> Props
formMethod = unsafeMkProps "formMethod"

formNoValidate :: Boolean -> Props
formNoValidate = unsafeMkProps "formNoValidate"

formTarget :: String -> Props
formTarget = unsafeMkProps "formTarget"

frameBorder :: String -> Props
frameBorder = unsafeMkProps "frameBorder"

headers :: String -> Props
headers = unsafeMkProps "headers"

height :: String -> Props
height = unsafeMkProps "height"

hidden :: Boolean -> Props
hidden = unsafeMkProps "hidden"

high :: String -> Props
high = unsafeMkProps "high"

href :: String -> Props
href = unsafeMkProps "href"

hrefLang :: String -> Props
hrefLang = unsafeMkProps "hrefLang"

htmlFor :: String -> Props
htmlFor = unsafeMkProps "htmlFor"

httpEquiv :: String -> Props
httpEquiv = unsafeMkProps "httpEquiv"

icon :: String -> Props
icon = unsafeMkProps "icon"

_id :: String -> Props
_id = unsafeMkProps "id"

inputMode :: String -> Props
inputMode = unsafeMkProps "inputMode"

integrity :: String -> Props
integrity = unsafeMkProps "integrity"

is :: String -> Props
is = unsafeMkProps "is"

key :: String -> Props
key = unsafeMkProps "key"

keyParams :: String -> Props
keyParams = unsafeMkProps "keyParams"

keyType :: String -> Props
keyType = unsafeMkProps "keyType"

kind :: String -> Props
kind = unsafeMkProps "kind"

label :: String -> Props
label = unsafeMkProps "label"

lang :: String -> Props
lang = unsafeMkProps "lang"

list :: String -> Props
list = unsafeMkProps "list"

loop :: Boolean -> Props
loop = unsafeMkProps "loop"

low :: String -> Props
low = unsafeMkProps "low"

manifest :: String -> Props
manifest = unsafeMkProps "manifest"

marginHeight :: String -> Props
marginHeight = unsafeMkProps "marginHeight"

marginWidth :: String -> Props
marginWidth = unsafeMkProps "marginWidth"

max :: String -> Props
max = unsafeMkProps "max"

maxLength :: String -> Props
maxLength = unsafeMkProps "maxLength"

media :: String -> Props
media = unsafeMkProps "media"

mediaGroup :: String -> Props
mediaGroup = unsafeMkProps "mediaGroup"

method :: String -> Props
method = unsafeMkProps "method"

min :: String -> Props
min = unsafeMkProps "min"

minLength :: String -> Props
minLength = unsafeMkProps "minLength"

multiple :: Boolean -> Props
multiple = unsafeMkProps "multiple"

muted :: Boolean -> Props
muted = unsafeMkProps "muted"

name :: String -> Props
name = unsafeMkProps "name"

nonce :: String -> Props
nonce = unsafeMkProps "nonce"

noValidate :: Boolean -> Props
noValidate = unsafeMkProps "noValidate"

open :: Boolean -> Props
open = unsafeMkProps "open"

optimum :: String -> Props
optimum = unsafeMkProps "optimum"

pattern :: String -> Props
pattern = unsafeMkProps "pattern"

placeholder :: String -> Props
placeholder = unsafeMkProps "placeholder"

poster :: String -> Props
poster = unsafeMkProps "poster"

preload :: String -> Props
preload = unsafeMkProps "preload"

profile :: String -> Props
profile = unsafeMkProps "profile"

radioGroup :: String -> Props
radioGroup = unsafeMkProps "radioGroup"

readOnly :: Boolean -> Props
readOnly = unsafeMkProps "readOnly"

rel :: String -> Props
rel = unsafeMkProps "rel"

required :: Boolean -> Props
required = unsafeMkProps "required"

reversed :: Boolean -> Props
reversed = unsafeMkProps "reversed"

role :: String -> Props
role = unsafeMkProps "role"

rows :: Int -> Props
rows = unsafeMkProps "rows"

rowSpan :: Int -> Props
rowSpan = unsafeMkProps "rowSpan"

sandbox :: String -> Props
sandbox = unsafeMkProps "sandbox"

scope :: String -> Props
scope = unsafeMkProps "scope"

scoped :: Boolean -> Props
scoped = unsafeMkProps "scoped"

scrolling :: String -> Props
scrolling = unsafeMkProps "scrolling"

seamless :: Boolean -> Props
seamless = unsafeMkProps "seamless"

selected :: Boolean -> Props
selected = unsafeMkProps "selected"

shape :: String -> Props
shape = unsafeMkProps "shape"

size :: Int -> Props
size = unsafeMkProps "size"

sizes :: String -> Props
sizes = unsafeMkProps "sizes"

span :: Int -> Props
span = unsafeMkProps "span"

spellCheck :: Boolean -> Props
spellCheck = unsafeMkProps "spellCheck"

src :: String -> Props
src = unsafeMkProps "src"

srcDoc :: String -> Props
srcDoc = unsafeMkProps "srcDoc"

srcLang :: String -> Props
srcLang = unsafeMkProps "srcLang"

srcSet :: String -> Props
srcSet = unsafeMkProps "srcSet"

start :: Int -> Props
start = unsafeMkProps "start"

step :: String -> Props
step = unsafeMkProps "step"

summary :: String -> Props
summary = unsafeMkProps "summary"

tabIndex :: Int -> Props
tabIndex = unsafeMkProps "tabIndex"

target :: String -> Props
target = unsafeMkProps "target"

title :: String -> Props
title = unsafeMkProps "title"

_type :: String -> Props
_type = unsafeMkProps "type"

useMap :: String -> Props
useMap = unsafeMkProps "useMap"

value :: String -> Props
value = unsafeMkProps "value"

width :: String -> Props
width = unsafeMkProps "width"

wmode :: String -> Props
wmode = unsafeMkProps "wmode"

wrap :: String -> Props
wrap = unsafeMkProps "wrap"

-- RDFa Attributes
about :: String -> Props
about = unsafeMkProps "about"

datatype :: String -> Props
datatype = unsafeMkProps "datatype"

inlist :: String -> Props
inlist = unsafeMkProps "inlist"

prefix :: String -> Props
prefix = unsafeMkProps "prefix"

property :: String -> Props
property = unsafeMkProps "property"

resource :: String -> Props
resource = unsafeMkProps "resource"

typeof :: String -> Props
typeof = unsafeMkProps "typeof"

vocab :: String -> Props
vocab = unsafeMkProps "vocab"

-- Non-standard Attributes
autoCapitalize :: String -> Props
autoCapitalize = unsafeMkProps "autoCapitalize"

autoCorrect :: String -> Props
autoCorrect = unsafeMkProps "autoCorrect"

autoSave :: String -> Props
autoSave = unsafeMkProps "autoSave"

color :: String -> Props
color = unsafeMkProps "color"

itemProp :: String -> Props
itemProp = unsafeMkProps "itemProp"

itemScope :: Boolean -> Props
itemScope = unsafeMkProps "itemScope"

itemType :: String -> Props
itemType = unsafeMkProps "itemType"

itemID :: String -> Props
itemID = unsafeMkProps "itemID"

itemRef :: String -> Props
itemRef = unsafeMkProps "itemRef"

results :: Int -> Props
results = unsafeMkProps "results"

security :: String -> Props
security = unsafeMkProps "security"

unselectable :: Boolean -> Props
unselectable = unsafeMkProps "unselectable"

onAnimationStart :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onAnimationStart f = unsafeMkProps "onAnimationStart" (handle f)

onAnimationEnd :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onAnimationEnd f = unsafeMkProps "onAnimationEnd" (handle f)

onAnimationIteration :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onAnimationIteration f = unsafeMkProps "onAnimationIteration" (handle f)

onTransitionEnd :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onTransitionEnd f = unsafeMkProps "onTransitionEnd" (handle f)

onLoad :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onLoad f = unsafeMkProps "onLoad" (handle f)

onCopy :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onCopy f = unsafeMkProps "onCopy" (handle f)

onCut :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onCut f = unsafeMkProps "onCut" (handle f)

onPaste :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onPaste f = unsafeMkProps "onPaste" (handle f)

onKeyDown :: forall eff props state result.
  (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
onKeyDown f = unsafeMkProps "onKeyDown" (handle f)

onKeyPress :: forall eff props state result.
  (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
onKeyPress f = unsafeMkProps "onKeyPress" (handle f)

onKeyUp :: forall eff props state result.
  (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
onKeyUp f = unsafeMkProps "onKeyUp" (handle f)

onFocus :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onFocus f = unsafeMkProps "onFocus" (handle f)

onBlur :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onBlur f = unsafeMkProps "onBlur" (handle f)

onChange :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onChange f = unsafeMkProps "onChange" (handle f)

onInput :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onInput f = unsafeMkProps "onInput" (handle f)

onInvalid :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onInvalid f = unsafeMkProps "onInvalid" (handle f)

onSubmit :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onSubmit f = unsafeMkProps "onSubmit" (handle f)

onClick :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onClick f = unsafeMkProps "onClick" (handle f)

onDoubleClick :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDoubleClick f = unsafeMkProps "onDoubleClick" (handle f)

onDrag :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDrag f = unsafeMkProps "onDrag" (handle f)

onDragEnd :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDragEnd f = unsafeMkProps "onDragEnd" (handle f)

onDragEnter :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDragEnter f = unsafeMkProps "onDragEnter" (handle f)

onDragExit :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDragExit f = unsafeMkProps "onDragExit" (handle f)

onDragLeave :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDragLeave f = unsafeMkProps "onDragLeave" (handle f)

onDragOver :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDragOver f = unsafeMkProps "onDragOver" (handle f)

onDragStart :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDragStart f = unsafeMkProps "onDragStart" (handle f)

onDrop :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onDrop f = unsafeMkProps "onDrop" (handle f)

onMouseDown :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseDown f = unsafeMkProps "onMouseDown" (handle f)

onMouseEnter :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseEnter f = unsafeMkProps "onMouseEnter" (handle f)

onMouseLeave :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseLeave f = unsafeMkProps "onMouseLeave" (handle f)

onMouseMove :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseMove f = unsafeMkProps "onMouseMove" (handle f)

onMouseOut :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseOut f = unsafeMkProps "onMouseOut" (handle f)

onMouseOver :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseOver f = unsafeMkProps "onMouseOver" (handle f)

onMouseUp :: forall eff props state result.
  (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseUp f = unsafeMkProps "onMouseUp" (handle f)

onTouchCancel :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onTouchCancel f = unsafeMkProps "onTouchCancel" (handle f)

onTouchEnd :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onTouchEnd f = unsafeMkProps "onTouchEnd" (handle f)

onTouchMove :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onTouchMove f = unsafeMkProps "onTouchMove" (handle f)

onTouchStart :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onTouchStart f = unsafeMkProps "onTouchStart" (handle f)

onScroll :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onScroll f = unsafeMkProps "onScroll" (handle f)

onWheel :: forall eff props state result.
  (Event -> EventHandlerContext eff props state result) -> Props
onWheel f = unsafeMkProps "onWheel" (handle f)

ref :: forall eff props state result.
  (Nullable Foreign -> EventHandlerContext eff props state result) -> Props
ref f = unsafeMkProps "ref" (handle f)

suppressContentEditableWarning :: Boolean -> Props
suppressContentEditableWarning = unsafeMkProps "suppressContentEditableWarning"

-- SVG attributes
x :: Int -> Props
x = unsafeMkProps "x"

y :: Int -> Props
y = unsafeMkProps "y"

cx :: Int -> Props
cx = unsafeMkProps "cx"

cy :: Int -> Props
cy = unsafeMkProps "cy"

r :: Int -> Props
r = unsafeMkProps "r"

fill :: String -> Props
fill = unsafeMkProps "fill"

opacity :: Int -> Props
opacity = unsafeMkProps "opacity"

fillOpacity :: Int -> Props
fillOpacity = unsafeMkProps "fillOpacity"

stroke :: String -> Props
stroke = unsafeMkProps "stroke"

strokeWidth :: Int -> Props
strokeWidth = unsafeMkProps "strokeWidth"

points :: String -> Props
points = unsafeMkProps "points"

d :: String -> Props
d = unsafeMkProps "d"

viewBox :: String -> Props
viewBox = unsafeMkProps "viewBox"
