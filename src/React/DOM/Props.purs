module React.DOM.Props where

import React (Event(), EventHandlerContext(), KeyboardEvent(), MouseEvent(), handle)

foreign import data Props :: *

foreign import unsafeMkProps :: forall val. String -> val -> Props

foreign import unsafeUnfoldProps :: forall vals. String -> { | vals } -> Props

foreign import unsafeFromPropsArray :: forall props. Array Props -> props

aria :: forall ariaAttrs. { | ariaAttrs } -> Props
aria = unsafeUnfoldProps "aria"

_data :: forall dataAttrs. { | dataAttrs } -> Props
_data = unsafeUnfoldProps "data"

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

allowFullScreen :: String -> Props
allowFullScreen = unsafeMkProps "allowFullScreen"

allowTransparency :: String -> Props
allowTransparency = unsafeMkProps "allowTransparency"

alt :: String -> Props
alt = unsafeMkProps "alt"

async :: String -> Props
async = unsafeMkProps "async"

autoComplete :: String -> Props
autoComplete = unsafeMkProps "autoComplete"

autoFocus :: Boolean -> Props
autoFocus = unsafeMkProps "autoFocus"

autoPlay :: String -> Props
autoPlay = unsafeMkProps "autoPlay"

cellPadding :: String -> Props
cellPadding = unsafeMkProps "cellPadding"

cellSpacing :: String -> Props
cellSpacing = unsafeMkProps "cellSpacing"

charSet :: String -> Props
charSet = unsafeMkProps "charSet"

checked :: String -> Props
checked = unsafeMkProps "checked"

classID :: String -> Props
classID = unsafeMkProps "classID"

className :: String -> Props
className = unsafeMkProps "className"

cols :: String -> Props
cols = unsafeMkProps "cols"

colSpan :: String -> Props
colSpan = unsafeMkProps "colSpan"

content :: String -> Props
content = unsafeMkProps "content"

contentEditable :: String -> Props
contentEditable = unsafeMkProps "contentEditable"

contextMenu :: String -> Props
contextMenu = unsafeMkProps "contextMenu"

controls :: String -> Props
controls = unsafeMkProps "controls"

coords :: String -> Props
coords = unsafeMkProps "coords"

crossOrigin :: String -> Props
crossOrigin = unsafeMkProps "crossOrigin"

dateTime :: String -> Props
dateTime = unsafeMkProps "dateTime"

defer :: String -> Props
defer = unsafeMkProps "defer"

dir :: String -> Props
dir = unsafeMkProps "dir"

disabled :: Boolean -> Props
disabled = unsafeMkProps "disabled"

download :: String -> Props
download = unsafeMkProps "download"

draggable :: String -> Props
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

formNoValidate :: String -> Props
formNoValidate = unsafeMkProps "formNoValidate"

formTarget :: String -> Props
formTarget = unsafeMkProps "formTarget"

frameBorder :: String -> Props
frameBorder = unsafeMkProps "frameBorder"

height :: String -> Props
height = unsafeMkProps "height"

hidden :: String -> Props
hidden = unsafeMkProps "hidden"

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

key :: String -> Props
key = unsafeMkProps "key"

label :: String -> Props
label = unsafeMkProps "label"

lang :: String -> Props
lang = unsafeMkProps "lang"

list :: String -> Props
list = unsafeMkProps "list"

loop :: String -> Props
loop = unsafeMkProps "loop"

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

multiple :: String -> Props
multiple = unsafeMkProps "multiple"

muted :: String -> Props
muted = unsafeMkProps "muted"

name :: String -> Props
name = unsafeMkProps "name"

noValidate :: String -> Props
noValidate = unsafeMkProps "noValidate"

open :: String -> Props
open = unsafeMkProps "open"

pattern :: String -> Props
pattern = unsafeMkProps "pattern"

placeholder :: String -> Props
placeholder = unsafeMkProps "placeholder"

poster :: String -> Props
poster = unsafeMkProps "poster"

preload :: String -> Props
preload = unsafeMkProps "preload"

radioGroup :: String -> Props
radioGroup = unsafeMkProps "radioGroup"

readOnly :: String -> Props
readOnly = unsafeMkProps "readOnly"

rel :: String -> Props
rel = unsafeMkProps "rel"

required :: String -> Props
required = unsafeMkProps "required"

role :: String -> Props
role = unsafeMkProps "role"

rows :: String -> Props
rows = unsafeMkProps "rows"

rowSpan :: String -> Props
rowSpan = unsafeMkProps "rowSpan"

sandbox :: String -> Props
sandbox = unsafeMkProps "sandbox"

scope :: String -> Props
scope = unsafeMkProps "scope"

scrolling :: String -> Props
scrolling = unsafeMkProps "scrolling"

seamless :: String -> Props
seamless = unsafeMkProps "seamless"

selected :: String -> Props
selected = unsafeMkProps "selected"

shape :: String -> Props
shape = unsafeMkProps "shape"

size :: String -> Props
size = unsafeMkProps "size"

sizes :: String -> Props
sizes = unsafeMkProps "sizes"

span :: String -> Props
span = unsafeMkProps "span"

spellCheck :: String -> Props
spellCheck = unsafeMkProps "spellCheck"

src :: String -> Props
src = unsafeMkProps "src"

srcDoc :: String -> Props
srcDoc = unsafeMkProps "srcDoc"

srcSet :: String -> Props
srcSet = unsafeMkProps "srcSet"

start :: String -> Props
start = unsafeMkProps "start"

step :: String -> Props
step = unsafeMkProps "step"

tabIndex :: String -> Props
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

onCopy :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onCopy f = unsafeMkProps "onCopy" (handle f)

onCut :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onCut f = unsafeMkProps "onCut" (handle f)

onPaste :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onPaste f = unsafeMkProps "onPaste" (handle f)

onKeyDown :: forall eff props state result. (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
onKeyDown f = unsafeMkProps "onKeyDown" (handle f)

onKeyPress :: forall eff props state result. (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
onKeyPress f = unsafeMkProps "onKeyPress" (handle f)

onKeyUp :: forall eff props state result. (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
onKeyUp f = unsafeMkProps "onKeyUp" (handle f)

onFocus :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onFocus f = unsafeMkProps "onFocus" (handle f)

onBlur :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onBlur f = unsafeMkProps "onBlur" (handle f)

onChange :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onChange f = unsafeMkProps "onChange" (handle f)

onInput :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onInput f = unsafeMkProps "onInput" (handle f)

onSubmit :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onSubmit f = unsafeMkProps "onSubmit" (handle f)

onClick :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onClick f = unsafeMkProps "onClick" (handle f)

onDoubleClick :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDoubleClick f = unsafeMkProps "onDoubleClick" (handle f)

onDrag :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDrag f = unsafeMkProps "onDrag" (handle f)

onDragEnd :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDragEnd f = unsafeMkProps "onDragEnd" (handle f)

onDragEnter :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDragEnter f = unsafeMkProps "onDragEnter" (handle f)

onDragExit :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDragExit f = unsafeMkProps "onDragExit" (handle f)

onDragLeave :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDragLeave f = unsafeMkProps "onDragLeave" (handle f)

onDragOver :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDragOver f = unsafeMkProps "onDragOver" (handle f)

onDragStart :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDragStart f = unsafeMkProps "onDragStart" (handle f)

onDrop :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onDrop f = unsafeMkProps "onDrop" (handle f)

onMouseDown :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseDown f = unsafeMkProps "onMouseDown" (handle f)

onMouseEnter :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseEnter f = unsafeMkProps "onMouseEnter" (handle f)

onMouseLeave :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseLeave f = unsafeMkProps "onMouseLeave" (handle f)

onMouseMove :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseMove f = unsafeMkProps "onMouseMove" (handle f)

onMouseOut :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseOut f = unsafeMkProps "onMouseOut" (handle f)

onMouseOver :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseOver f = unsafeMkProps "onMouseOver" (handle f)

onMouseUp :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
onMouseUp f = unsafeMkProps "onMouseUp" (handle f)

onTouchCancel :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onTouchCancel f = unsafeMkProps "onTouchCancel" (handle f)

onTouchEnd :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onTouchEnd f = unsafeMkProps "onTouchEnd" (handle f)

onTouchMove :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onTouchMove f = unsafeMkProps "onTouchMove" (handle f)

onTouchStart :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onTouchStart f = unsafeMkProps "onTouchStart" (handle f)

onScroll :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onScroll f = unsafeMkProps "onScroll" (handle f)

onWheel :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
onWheel f = unsafeMkProps "onWheel" (handle f)
