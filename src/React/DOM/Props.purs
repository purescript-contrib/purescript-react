module React.DOM.Props where

import Data.Nullable (Nullable)

import React (SyntheticEventHandlerContext, Ref, handle)

import React.SyntheticEvent
  ( SyntheticEvent
  , SyntheticAnimationEvent
  , SyntheticClipboardEvent
  , SyntheticCompositionEvent
  , SyntheticInputEvent
  , SyntheticKeyboardEvent
  , SyntheticFocusEvent
  , SyntheticMouseEvent
  , SyntheticTouchEvent
  , SyntheticTransitionEvent
  , SyntheticUIEvent
  , SyntheticWheelEvent
  )

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

valueArray :: Array String -> Props
valueArray = unsafeMkProps "value"

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
  (SyntheticAnimationEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAnimationStart f = unsafeMkProps "onAnimationStart" (handle f)

onAnimationEnd :: forall eff props state result.
  (SyntheticAnimationEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAnimationEnd f = unsafeMkProps "onAnimationEnd" (handle f)

onAnimationIteration :: forall eff props state result.
  (SyntheticAnimationEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAnimationIteration f = unsafeMkProps "onAnimationIteration" (handle f)

onTransitionEnd :: forall eff props state result.
  (SyntheticTransitionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTransitionEnd f = unsafeMkProps "onTransitionEnd" (handle f)

onToggle :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onToggle f = unsafeMkProps "onToggle" (handle f)

onError :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onError f = unsafeMkProps "onError" (handle f)

onLoad :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoad f = unsafeMkProps "onLoad" (handle f)

onAbort :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAbort f = unsafeMkProps "onAbort" (handle f)

onCanPlay :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCanPlay f = unsafeMkProps "onCanPlay" (handle f)

onCanPlayThrough :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCanPlayThrough f = unsafeMkProps "onCanPlayThrough" (handle f)

onDurationChange :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDurationChange f = unsafeMkProps "onDurationChange" (handle f)

onEmptied :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onEmptied f = unsafeMkProps "onEmptied" (handle f)

onEncrypted :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onEncrypted f = unsafeMkProps "onEncrypted" (handle f)

onEnded :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onEnded f = unsafeMkProps "onEnded" (handle f)

onLoadedData :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoadedData f = unsafeMkProps "onLoadedData" (handle f)

onLoadedMetadata :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoadedMetadata f = unsafeMkProps "onLoadedMetadata" (handle f)

onLoadStart :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoadStart f = unsafeMkProps "onLoadStart" (handle f)

onPause :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPause f = unsafeMkProps "onPause" (handle f)

onPlay :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPlay f = unsafeMkProps "onPlay" (handle f)

onPlaying :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPlaying f = unsafeMkProps "onPlaying" (handle f)

onProgress :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onProgress f = unsafeMkProps "onProgress" (handle f)

onRateChange :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onRateChange f = unsafeMkProps "onRateChange" (handle f)

onSeeked :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSeeked f = unsafeMkProps "onSeeked" (handle f)

onSeeking :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSeeking f = unsafeMkProps "onSeeking" (handle f)

onStalled :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onStalled f = unsafeMkProps "onStalled" (handle f)

onSuspend :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSuspend f = unsafeMkProps "onSuspend" (handle f)

onTimeUpdate :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTimeUpdate f = unsafeMkProps "onTimeUpdate" (handle f)

onVolumeChange :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onVolumeChange f = unsafeMkProps "onVolumeChange" (handle f)

onWaiting :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onWaiting f = unsafeMkProps "onWaiting" (handle f)

onCopy :: forall eff props state result.
  (SyntheticClipboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCopy f = unsafeMkProps "onCopy" (handle f)

onCut :: forall eff props state result.
  (SyntheticClipboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCut f = unsafeMkProps "onCut" (handle f)

onPaste :: forall eff props state result.
  (SyntheticClipboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPaste f = unsafeMkProps "onPaste" (handle f)

onCompositionEnd :: forall eff props state result.
  (SyntheticCompositionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCompositionEnd f = unsafeMkProps "onCompositionEnd" (handle f)

onCompositionStart :: forall eff props state result.
  (SyntheticCompositionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCompositionStart f = unsafeMkProps "onCompositionStart" (handle f)

onCompositionUpdate :: forall eff props state result.
  (SyntheticCompositionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCompositionUpdate f = unsafeMkProps "onCompositionUpdate" (handle f)

onKeyDown :: forall eff props state result.
  (SyntheticKeyboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onKeyDown f = unsafeMkProps "onKeyDown" (handle f)

onKeyPress :: forall eff props state result.
  (SyntheticKeyboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onKeyPress f = unsafeMkProps "onKeyPress" (handle f)

onKeyUp :: forall eff props state result.
  (SyntheticKeyboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onKeyUp f = unsafeMkProps "onKeyUp" (handle f)

onFocus :: forall eff props state result.
  (SyntheticFocusEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onFocus f = unsafeMkProps "onFocus" (handle f)

onBlur :: forall eff props state result.
  (SyntheticFocusEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onBlur f = unsafeMkProps "onBlur" (handle f)

onChange :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onChange f = unsafeMkProps "onChange" (handle f)

onInput :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onInput f = unsafeMkProps "onInput" (handle f)

onInvalid :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onInvalid f = unsafeMkProps "onInvalid" (handle f)

onSubmit :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSubmit f = unsafeMkProps "onSubmit" (handle f)

onClick :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onClick f = unsafeMkProps "onClick" (handle f)

onContextMenu :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onContextMenu f = unsafeMkProps "onContextMenu" (handle f)

onDoubleClick :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDoubleClick f = unsafeMkProps "onDoubleClick" (handle f)

onDrag :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDrag f = unsafeMkProps "onDrag" (handle f)

onDragEnd :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragEnd f = unsafeMkProps "onDragEnd" (handle f)

onDragEnter :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragEnter f = unsafeMkProps "onDragEnter" (handle f)

onDragExit :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragExit f = unsafeMkProps "onDragExit" (handle f)

onDragLeave :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragLeave f = unsafeMkProps "onDragLeave" (handle f)

onDragOver :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragOver f = unsafeMkProps "onDragOver" (handle f)

onDragStart :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragStart f = unsafeMkProps "onDragStart" (handle f)

onDrop :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDrop f = unsafeMkProps "onDrop" (handle f)

onMouseDown :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseDown f = unsafeMkProps "onMouseDown" (handle f)

onMouseEnter :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseEnter f = unsafeMkProps "onMouseEnter" (handle f)

onMouseLeave :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseLeave f = unsafeMkProps "onMouseLeave" (handle f)

onMouseMove :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseMove f = unsafeMkProps "onMouseMove" (handle f)

onMouseOut :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseOut f = unsafeMkProps "onMouseOut" (handle f)

onMouseOver :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseOver f = unsafeMkProps "onMouseOver" (handle f)

onMouseUp :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseUp f = unsafeMkProps "onMouseUp" (handle f)

onSelect :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSelect f = unsafeMkProps "onSelect" (handle f)

onTouchCancel :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchCancel f = unsafeMkProps "onTouchCancel" (handle f)

onTouchEnd :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchEnd f = unsafeMkProps "onTouchEnd" (handle f)

onTouchMove :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchMove f = unsafeMkProps "onTouchMove" (handle f)

onTouchStart :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchStart f = unsafeMkProps "onTouchStart" (handle f)

onScroll :: forall eff props state result.
  (SyntheticUIEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onScroll f = unsafeMkProps "onScroll" (handle f)

onWheel :: forall eff props state result.
  (SyntheticWheelEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onWheel f = unsafeMkProps "onWheel" (handle f)

onAnimationStartCapture :: forall eff props state result.
  (SyntheticAnimationEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAnimationStartCapture f = unsafeMkProps "onAnimationStartCapture" (handle f)

onAnimationEndCapture :: forall eff props state result.
  (SyntheticAnimationEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAnimationEndCapture f = unsafeMkProps "onAnimationEndCapture" (handle f)

onAnimationIterationCapture :: forall eff props state result.
  (SyntheticAnimationEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAnimationIterationCapture f = unsafeMkProps "onAnimationIterationCapture" (handle f)

onTransitionEndCapture :: forall eff props state result.
  (SyntheticTransitionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTransitionEndCapture f = unsafeMkProps "onTransitionEndCapture" (handle f)

onToggleCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onToggleCapture f = unsafeMkProps "onToggleCapture" (handle f)

onErrorCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onErrorCapture f = unsafeMkProps "onErrorCapture" (handle f)

onLoadCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoadCapture f = unsafeMkProps "onLoadCapture" (handle f)

onAbortCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onAbortCapture f = unsafeMkProps "onAbortCapture" (handle f)

onCanPlayCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCanPlayCapture f = unsafeMkProps "onCanPlayCapture" (handle f)

onCanPlayThroughCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCanPlayThroughCapture f = unsafeMkProps "onCanPlayThroughCapture" (handle f)

onDurationChangeCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDurationChangeCapture f = unsafeMkProps "onDurationChangeCapture" (handle f)

onEmptiedCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onEmptiedCapture f = unsafeMkProps "onEmptiedCapture" (handle f)

onEncryptedCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onEncryptedCapture f = unsafeMkProps "onEncryptedCapture" (handle f)

onEndedCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onEndedCapture f = unsafeMkProps "onEndedCapture" (handle f)

onLoadedDataCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoadedDataCapture f = unsafeMkProps "onLoadedDataCapture" (handle f)

onLoadedMetadataCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoadedMetadataCapture f = unsafeMkProps "onLoadedMetadataCapture" (handle f)

onLoadStartCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onLoadStartCapture f = unsafeMkProps "onLoadStartCapture" (handle f)

onPauseCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPauseCapture f = unsafeMkProps "onPauseCapture" (handle f)

onPlayCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPlayCapture f = unsafeMkProps "onPlayCapture" (handle f)

onPlayingCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPlayingCapture f = unsafeMkProps "onPlayingCapture" (handle f)

onProgressCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onProgressCapture f = unsafeMkProps "onProgressCapture" (handle f)

onRateChangeCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onRateChangeCapture f = unsafeMkProps "onRateChangeCapture" (handle f)

onSeekedCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSeekedCapture f = unsafeMkProps "onSeekedCapture" (handle f)

onSeekingCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSeekingCapture f = unsafeMkProps "onSeekingCapture" (handle f)

onStalledCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onStalledCapture f = unsafeMkProps "onStalledCapture" (handle f)

onSuspendCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSuspendCapture f = unsafeMkProps "onSuspendCapture" (handle f)

onTimeUpdateCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTimeUpdateCapture f = unsafeMkProps "onTimeUpdateCapture" (handle f)

onVolumeChangeCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onVolumeChangeCapture f = unsafeMkProps "onVolumeChangeCapture" (handle f)

onWaitingCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onWaitingCapture f = unsafeMkProps "onWaitingCapture" (handle f)

onCopyCapture :: forall eff props state result.
  (SyntheticClipboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCopyCapture f = unsafeMkProps "onCopyCapture" (handle f)

onCutCapture :: forall eff props state result.
  (SyntheticClipboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCutCapture f = unsafeMkProps "onCutCapture" (handle f)

onPasteCapture :: forall eff props state result.
  (SyntheticClipboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onPasteCapture f = unsafeMkProps "onPasteCapture" (handle f)

onCompositionEndCapture :: forall eff props state result.
  (SyntheticCompositionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCompositionEndCapture f = unsafeMkProps "onCompositionEndCapture" (handle f)

onCompositionStartCapture :: forall eff props state result.
  (SyntheticCompositionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCompositionStartCapture f = unsafeMkProps "onCompositionStartCapture" (handle f)

onCompositionUpdateCapture :: forall eff props state result.
  (SyntheticCompositionEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onCompositionUpdateCapture f = unsafeMkProps "onCompositionUpdateCapture" (handle f)

onKeyDownCapture :: forall eff props state result.
  (SyntheticKeyboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onKeyDownCapture f = unsafeMkProps "onKeyDownCapture" (handle f)

onKeyPressCapture :: forall eff props state result.
  (SyntheticKeyboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onKeyPressCapture f = unsafeMkProps "onKeyPressCapture" (handle f)

onKeyUpCapture :: forall eff props state result.
  (SyntheticKeyboardEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onKeyUpCapture f = unsafeMkProps "onKeyUpCapture" (handle f)

onFocusCapture :: forall eff props state result.
  (SyntheticFocusEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onFocusCapture f = unsafeMkProps "onFocusCapture" (handle f)

onBlurCapture :: forall eff props state result.
  (SyntheticFocusEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onBlurCapture f = unsafeMkProps "onBlurCapture" (handle f)

onChangeCapture :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onChangeCapture f = unsafeMkProps "onChangeCapture" (handle f)

onInputCapture :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onInputCapture f = unsafeMkProps "onInputCapture" (handle f)

onInvalidCapture :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onInvalidCapture f = unsafeMkProps "onInvalidCapture" (handle f)

onSubmitCapture :: forall eff props state result.
  (SyntheticInputEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSubmitCapture f = unsafeMkProps "onSubmitCapture" (handle f)

onClickCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onClickCapture f = unsafeMkProps "onClickCapture" (handle f)

onContextMenuCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onContextMenuCapture f = unsafeMkProps "onContextMenuCapture" (handle f)

onDoubleClickCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDoubleClickCapture f = unsafeMkProps "onDoubleClickCapture" (handle f)

onDragCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragCapture f = unsafeMkProps "onDragCapture" (handle f)

onDragEndCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragEndCapture f = unsafeMkProps "onDragEndCapture" (handle f)

onDragEnterCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragEnterCapture f = unsafeMkProps "onDragEnterCapture" (handle f)

onDragExitCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragExitCapture f = unsafeMkProps "onDragExitCapture" (handle f)

onDragLeaveCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragLeaveCapture f = unsafeMkProps "onDragLeaveCapture" (handle f)

onDragOverCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragOverCapture f = unsafeMkProps "onDragOverCapture" (handle f)

onDragStartCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDragStartCapture f = unsafeMkProps "onDragStartCapture" (handle f)

onDropCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onDropCapture f = unsafeMkProps "onDropCapture" (handle f)

onMouseDownCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseDownCapture f = unsafeMkProps "onMouseDownCapture" (handle f)

onMouseEnterCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseEnterCapture f = unsafeMkProps "onMouseEnterCapture" (handle f)

onMouseLeaveCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseLeaveCapture f = unsafeMkProps "onMouseLeaveCapture" (handle f)

onMouseMoveCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseMoveCapture f = unsafeMkProps "onMouseMoveCapture" (handle f)

onMouseOutCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseOutCapture f = unsafeMkProps "onMouseOutCapture" (handle f)

onMouseOverCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseOverCapture f = unsafeMkProps "onMouseOverCapture" (handle f)

onMouseUpCapture :: forall eff props state result.
  (SyntheticMouseEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onMouseUpCapture f = unsafeMkProps "onMouseUpCapture" (handle f)

onSelectCapture :: forall eff props state result.
  (SyntheticEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onSelectCapture f = unsafeMkProps "onSelectCapture" (handle f)

onTouchCancelCapture :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchCancelCapture f = unsafeMkProps "onTouchCancelCapture" (handle f)

onTouchEndCapture :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchEndCapture f = unsafeMkProps "onTouchEndCapture" (handle f)

onTouchMoveCapture :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchMoveCapture f = unsafeMkProps "onTouchMoveCapture" (handle f)

onTouchStartCapture :: forall eff props state result.
  (SyntheticTouchEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onTouchStartCapture f = unsafeMkProps "onTouchStartCapture" (handle f)

onScrollCapture :: forall eff props state result.
  (SyntheticUIEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onScrollCapture f = unsafeMkProps "onScrollCapture" (handle f)

onWheelCapture :: forall eff props state result.
  (SyntheticWheelEvent -> SyntheticEventHandlerContext eff props state result) -> Props
onWheelCapture f = unsafeMkProps "onWheelCapture" (handle f)

ref :: forall eff props state result.
  (Nullable Ref -> SyntheticEventHandlerContext eff props state result) -> Props
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
