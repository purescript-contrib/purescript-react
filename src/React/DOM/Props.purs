module React.DOM.Props where

import Prelude

import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Ref as Ref
import React.SyntheticEvent
  ( SyntheticAnimationEvent
  , SyntheticClipboardEvent
  , SyntheticCompositionEvent
  , SyntheticEvent
  , SyntheticFocusEvent
  , SyntheticInputEvent
  , SyntheticKeyboardEvent
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

onAnimationStart :: (SyntheticAnimationEvent -> Effect Unit) -> Props
onAnimationStart f = unsafeMkProps "onAnimationStart" (mkEffectFn1 f)

onAnimationEnd :: (SyntheticAnimationEvent -> Effect Unit) -> Props
onAnimationEnd f = unsafeMkProps "onAnimationEnd" (mkEffectFn1 f)

onAnimationIteration :: (SyntheticAnimationEvent -> Effect Unit) -> Props
onAnimationIteration f = unsafeMkProps "onAnimationIteration" (mkEffectFn1 f)

onTransitionEnd :: (SyntheticTransitionEvent -> Effect Unit) -> Props
onTransitionEnd f = unsafeMkProps "onTransitionEnd" (mkEffectFn1 f)

onToggle :: (SyntheticEvent -> Effect Unit) -> Props
onToggle f = unsafeMkProps "onToggle" (mkEffectFn1 f)

onError :: (SyntheticEvent -> Effect Unit) -> Props
onError f = unsafeMkProps "onError" (mkEffectFn1 f)

onLoad :: (SyntheticEvent -> Effect Unit) -> Props
onLoad f = unsafeMkProps "onLoad" (mkEffectFn1 f)

onAbort :: (SyntheticEvent -> Effect Unit) -> Props
onAbort f = unsafeMkProps "onAbort" (mkEffectFn1 f)

onCanPlay :: (SyntheticEvent -> Effect Unit) -> Props
onCanPlay f = unsafeMkProps "onCanPlay" (mkEffectFn1 f)

onCanPlayThrough :: (SyntheticEvent -> Effect Unit) -> Props
onCanPlayThrough f = unsafeMkProps "onCanPlayThrough" (mkEffectFn1 f)

onDurationChange :: (SyntheticEvent -> Effect Unit) -> Props
onDurationChange f = unsafeMkProps "onDurationChange" (mkEffectFn1 f)

onEmptied :: (SyntheticEvent -> Effect Unit) -> Props
onEmptied f = unsafeMkProps "onEmptied" (mkEffectFn1 f)

onEncrypted :: (SyntheticEvent -> Effect Unit) -> Props
onEncrypted f = unsafeMkProps "onEncrypted" (mkEffectFn1 f)

onEnded :: (SyntheticEvent -> Effect Unit) -> Props
onEnded f = unsafeMkProps "onEnded" (mkEffectFn1 f)

onLoadedData :: (SyntheticEvent -> Effect Unit) -> Props
onLoadedData f = unsafeMkProps "onLoadedData" (mkEffectFn1 f)

onLoadedMetadata :: (SyntheticEvent -> Effect Unit) -> Props
onLoadedMetadata f = unsafeMkProps "onLoadedMetadata" (mkEffectFn1 f)

onLoadStart :: (SyntheticEvent -> Effect Unit) -> Props
onLoadStart f = unsafeMkProps "onLoadStart" (mkEffectFn1 f)

onPause :: (SyntheticEvent -> Effect Unit) -> Props
onPause f = unsafeMkProps "onPause" (mkEffectFn1 f)

onPlay :: (SyntheticEvent -> Effect Unit) -> Props
onPlay f = unsafeMkProps "onPlay" (mkEffectFn1 f)

onPlaying :: (SyntheticEvent -> Effect Unit) -> Props
onPlaying f = unsafeMkProps "onPlaying" (mkEffectFn1 f)

onProgress :: (SyntheticEvent -> Effect Unit) -> Props
onProgress f = unsafeMkProps "onProgress" (mkEffectFn1 f)

onRateChange :: (SyntheticEvent -> Effect Unit) -> Props
onRateChange f = unsafeMkProps "onRateChange" (mkEffectFn1 f)

onSeeked :: (SyntheticEvent -> Effect Unit) -> Props
onSeeked f = unsafeMkProps "onSeeked" (mkEffectFn1 f)

onSeeking :: (SyntheticEvent -> Effect Unit) -> Props
onSeeking f = unsafeMkProps "onSeeking" (mkEffectFn1 f)

onStalled :: (SyntheticEvent -> Effect Unit) -> Props
onStalled f = unsafeMkProps "onStalled" (mkEffectFn1 f)

onSuspend :: (SyntheticEvent -> Effect Unit) -> Props
onSuspend f = unsafeMkProps "onSuspend" (mkEffectFn1 f)

onTimeUpdate :: (SyntheticEvent -> Effect Unit) -> Props
onTimeUpdate f = unsafeMkProps "onTimeUpdate" (mkEffectFn1 f)

onVolumeChange :: (SyntheticEvent -> Effect Unit) -> Props
onVolumeChange f = unsafeMkProps "onVolumeChange" (mkEffectFn1 f)

onWaiting :: (SyntheticEvent -> Effect Unit) -> Props
onWaiting f = unsafeMkProps "onWaiting" (mkEffectFn1 f)

onCopy :: (SyntheticClipboardEvent -> Effect Unit) -> Props
onCopy f = unsafeMkProps "onCopy" (mkEffectFn1 f)

onCut :: (SyntheticClipboardEvent -> Effect Unit) -> Props
onCut f = unsafeMkProps "onCut" (mkEffectFn1 f)

onPaste :: (SyntheticClipboardEvent -> Effect Unit) -> Props
onPaste f = unsafeMkProps "onPaste" (mkEffectFn1 f)

onCompositionEnd :: (SyntheticCompositionEvent -> Effect Unit) -> Props
onCompositionEnd f = unsafeMkProps "onCompositionEnd" (mkEffectFn1 f)

onCompositionStart :: (SyntheticCompositionEvent -> Effect Unit) -> Props
onCompositionStart f = unsafeMkProps "onCompositionStart" (mkEffectFn1 f)

onCompositionUpdate :: (SyntheticCompositionEvent -> Effect Unit) -> Props
onCompositionUpdate f = unsafeMkProps "onCompositionUpdate" (mkEffectFn1 f)

onKeyDown :: (SyntheticKeyboardEvent -> Effect Unit) -> Props
onKeyDown f = unsafeMkProps "onKeyDown" (mkEffectFn1 f)

onKeyPress :: (SyntheticKeyboardEvent -> Effect Unit) -> Props
onKeyPress f = unsafeMkProps "onKeyPress" (mkEffectFn1 f)

onKeyUp :: (SyntheticKeyboardEvent -> Effect Unit) -> Props
onKeyUp f = unsafeMkProps "onKeyUp" (mkEffectFn1 f)

onFocus :: (SyntheticFocusEvent -> Effect Unit) -> Props
onFocus f = unsafeMkProps "onFocus" (mkEffectFn1 f)

onBlur :: (SyntheticFocusEvent -> Effect Unit) -> Props
onBlur f = unsafeMkProps "onBlur" (mkEffectFn1 f)

onChange :: (SyntheticInputEvent -> Effect Unit) -> Props
onChange f = unsafeMkProps "onChange" (mkEffectFn1 f)

onInput :: (SyntheticInputEvent -> Effect Unit) -> Props
onInput f = unsafeMkProps "onInput" (mkEffectFn1 f)

onInvalid :: (SyntheticInputEvent -> Effect Unit) -> Props
onInvalid f = unsafeMkProps "onInvalid" (mkEffectFn1 f)

onSubmit :: (SyntheticInputEvent -> Effect Unit) -> Props
onSubmit f = unsafeMkProps "onSubmit" (mkEffectFn1 f)

onClick :: (SyntheticMouseEvent -> Effect Unit) -> Props
onClick f = unsafeMkProps "onClick" (mkEffectFn1 f)

onContextMenu :: (SyntheticMouseEvent -> Effect Unit) -> Props
onContextMenu f = unsafeMkProps "onContextMenu" (mkEffectFn1 f)

onDoubleClick :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDoubleClick f = unsafeMkProps "onDoubleClick" (mkEffectFn1 f)

onDrag :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDrag f = unsafeMkProps "onDrag" (mkEffectFn1 f)

onDragEnd :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragEnd f = unsafeMkProps "onDragEnd" (mkEffectFn1 f)

onDragEnter :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragEnter f = unsafeMkProps "onDragEnter" (mkEffectFn1 f)

onDragExit :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragExit f = unsafeMkProps "onDragExit" (mkEffectFn1 f)

onDragLeave :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragLeave f = unsafeMkProps "onDragLeave" (mkEffectFn1 f)

onDragOver :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragOver f = unsafeMkProps "onDragOver" (mkEffectFn1 f)

onDragStart :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragStart f = unsafeMkProps "onDragStart" (mkEffectFn1 f)

onDrop :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDrop f = unsafeMkProps "onDrop" (mkEffectFn1 f)

onMouseDown :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseDown f = unsafeMkProps "onMouseDown" (mkEffectFn1 f)

onMouseEnter :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseEnter f = unsafeMkProps "onMouseEnter" (mkEffectFn1 f)

onMouseLeave :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseLeave f = unsafeMkProps "onMouseLeave" (mkEffectFn1 f)

onMouseMove :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseMove f = unsafeMkProps "onMouseMove" (mkEffectFn1 f)

onMouseOut :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseOut f = unsafeMkProps "onMouseOut" (mkEffectFn1 f)

onMouseOver :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseOver f = unsafeMkProps "onMouseOver" (mkEffectFn1 f)

onMouseUp :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseUp f = unsafeMkProps "onMouseUp" (mkEffectFn1 f)

onSelect :: (SyntheticEvent -> Effect Unit) -> Props
onSelect f = unsafeMkProps "onSelect" (mkEffectFn1 f)

onTouchCancel :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchCancel f = unsafeMkProps "onTouchCancel" (mkEffectFn1 f)

onTouchEnd :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchEnd f = unsafeMkProps "onTouchEnd" (mkEffectFn1 f)

onTouchMove :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchMove f = unsafeMkProps "onTouchMove" (mkEffectFn1 f)

onTouchStart :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchStart f = unsafeMkProps "onTouchStart" (mkEffectFn1 f)

onScroll :: (SyntheticUIEvent -> Effect Unit) -> Props
onScroll f = unsafeMkProps "onScroll" (mkEffectFn1 f)

onWheel :: (SyntheticWheelEvent -> Effect Unit) -> Props
onWheel f = unsafeMkProps "onWheel" (mkEffectFn1 f)

onAnimationStartCapture :: (SyntheticAnimationEvent -> Effect Unit) -> Props
onAnimationStartCapture f = unsafeMkProps "onAnimationStartCapture" (mkEffectFn1 f)

onAnimationEndCapture :: (SyntheticAnimationEvent -> Effect Unit) -> Props
onAnimationEndCapture f = unsafeMkProps "onAnimationEndCapture" (mkEffectFn1 f)

onAnimationIterationCapture :: (SyntheticAnimationEvent -> Effect Unit) -> Props
onAnimationIterationCapture f = unsafeMkProps "onAnimationIterationCapture" (mkEffectFn1 f)

onTransitionEndCapture :: (SyntheticTransitionEvent -> Effect Unit) -> Props
onTransitionEndCapture f = unsafeMkProps "onTransitionEndCapture" (mkEffectFn1 f)

onToggleCapture :: (SyntheticEvent -> Effect Unit) -> Props
onToggleCapture f = unsafeMkProps "onToggleCapture" (mkEffectFn1 f)

onErrorCapture :: (SyntheticEvent -> Effect Unit) -> Props
onErrorCapture f = unsafeMkProps "onErrorCapture" (mkEffectFn1 f)

onLoadCapture :: (SyntheticEvent -> Effect Unit) -> Props
onLoadCapture f = unsafeMkProps "onLoadCapture" (mkEffectFn1 f)

onAbortCapture :: (SyntheticEvent -> Effect Unit) -> Props
onAbortCapture f = unsafeMkProps "onAbortCapture" (mkEffectFn1 f)

onCanPlayCapture :: (SyntheticEvent -> Effect Unit) -> Props
onCanPlayCapture f = unsafeMkProps "onCanPlayCapture" (mkEffectFn1 f)

onCanPlayThroughCapture :: (SyntheticEvent -> Effect Unit) -> Props
onCanPlayThroughCapture f = unsafeMkProps "onCanPlayThroughCapture" (mkEffectFn1 f)

onDurationChangeCapture :: (SyntheticEvent -> Effect Unit) -> Props
onDurationChangeCapture f = unsafeMkProps "onDurationChangeCapture" (mkEffectFn1 f)

onEmptiedCapture :: (SyntheticEvent -> Effect Unit) -> Props
onEmptiedCapture f = unsafeMkProps "onEmptiedCapture" (mkEffectFn1 f)

onEncryptedCapture :: (SyntheticEvent -> Effect Unit) -> Props
onEncryptedCapture f = unsafeMkProps "onEncryptedCapture" (mkEffectFn1 f)

onEndedCapture :: (SyntheticEvent -> Effect Unit) -> Props
onEndedCapture f = unsafeMkProps "onEndedCapture" (mkEffectFn1 f)

onLoadedDataCapture :: (SyntheticEvent -> Effect Unit) -> Props
onLoadedDataCapture f = unsafeMkProps "onLoadedDataCapture" (mkEffectFn1 f)

onLoadedMetadataCapture :: (SyntheticEvent -> Effect Unit) -> Props
onLoadedMetadataCapture f = unsafeMkProps "onLoadedMetadataCapture" (mkEffectFn1 f)

onLoadStartCapture :: (SyntheticEvent -> Effect Unit) -> Props
onLoadStartCapture f = unsafeMkProps "onLoadStartCapture" (mkEffectFn1 f)

onPauseCapture :: (SyntheticEvent -> Effect Unit) -> Props
onPauseCapture f = unsafeMkProps "onPauseCapture" (mkEffectFn1 f)

onPlayCapture :: (SyntheticEvent -> Effect Unit) -> Props
onPlayCapture f = unsafeMkProps "onPlayCapture" (mkEffectFn1 f)

onPlayingCapture :: (SyntheticEvent -> Effect Unit) -> Props
onPlayingCapture f = unsafeMkProps "onPlayingCapture" (mkEffectFn1 f)

onProgressCapture :: (SyntheticEvent -> Effect Unit) -> Props
onProgressCapture f = unsafeMkProps "onProgressCapture" (mkEffectFn1 f)

onRateChangeCapture :: (SyntheticEvent -> Effect Unit) -> Props
onRateChangeCapture f = unsafeMkProps "onRateChangeCapture" (mkEffectFn1 f)

onSeekedCapture :: (SyntheticEvent -> Effect Unit) -> Props
onSeekedCapture f = unsafeMkProps "onSeekedCapture" (mkEffectFn1 f)

onSeekingCapture :: (SyntheticEvent -> Effect Unit) -> Props
onSeekingCapture f = unsafeMkProps "onSeekingCapture" (mkEffectFn1 f)

onStalledCapture :: (SyntheticEvent -> Effect Unit) -> Props
onStalledCapture f = unsafeMkProps "onStalledCapture" (mkEffectFn1 f)

onSuspendCapture :: (SyntheticEvent -> Effect Unit) -> Props
onSuspendCapture f = unsafeMkProps "onSuspendCapture" (mkEffectFn1 f)

onTimeUpdateCapture :: (SyntheticEvent -> Effect Unit) -> Props
onTimeUpdateCapture f = unsafeMkProps "onTimeUpdateCapture" (mkEffectFn1 f)

onVolumeChangeCapture :: (SyntheticEvent -> Effect Unit) -> Props
onVolumeChangeCapture f = unsafeMkProps "onVolumeChangeCapture" (mkEffectFn1 f)

onWaitingCapture :: (SyntheticEvent -> Effect Unit) -> Props
onWaitingCapture f = unsafeMkProps "onWaitingCapture" (mkEffectFn1 f)

onCopyCapture :: (SyntheticClipboardEvent -> Effect Unit) -> Props
onCopyCapture f = unsafeMkProps "onCopyCapture" (mkEffectFn1 f)

onCutCapture :: (SyntheticClipboardEvent -> Effect Unit) -> Props
onCutCapture f = unsafeMkProps "onCutCapture" (mkEffectFn1 f)

onPasteCapture :: (SyntheticClipboardEvent -> Effect Unit) -> Props
onPasteCapture f = unsafeMkProps "onPasteCapture" (mkEffectFn1 f)

onCompositionEndCapture :: (SyntheticCompositionEvent -> Effect Unit) -> Props
onCompositionEndCapture f = unsafeMkProps "onCompositionEndCapture" (mkEffectFn1 f)

onCompositionStartCapture :: (SyntheticCompositionEvent -> Effect Unit) -> Props
onCompositionStartCapture f = unsafeMkProps "onCompositionStartCapture" (mkEffectFn1 f)

onCompositionUpdateCapture :: (SyntheticCompositionEvent -> Effect Unit) -> Props
onCompositionUpdateCapture f = unsafeMkProps "onCompositionUpdateCapture" (mkEffectFn1 f)

onKeyDownCapture :: (SyntheticKeyboardEvent -> Effect Unit) -> Props
onKeyDownCapture f = unsafeMkProps "onKeyDownCapture" (mkEffectFn1 f)

onKeyPressCapture :: (SyntheticKeyboardEvent -> Effect Unit) -> Props
onKeyPressCapture f = unsafeMkProps "onKeyPressCapture" (mkEffectFn1 f)

onKeyUpCapture :: (SyntheticKeyboardEvent -> Effect Unit) -> Props
onKeyUpCapture f = unsafeMkProps "onKeyUpCapture" (mkEffectFn1 f)

onFocusCapture :: (SyntheticFocusEvent -> Effect Unit) -> Props
onFocusCapture f = unsafeMkProps "onFocusCapture" (mkEffectFn1 f)

onBlurCapture :: (SyntheticFocusEvent -> Effect Unit) -> Props
onBlurCapture f = unsafeMkProps "onBlurCapture" (mkEffectFn1 f)

onChangeCapture :: (SyntheticInputEvent -> Effect Unit) -> Props
onChangeCapture f = unsafeMkProps "onChangeCapture" (mkEffectFn1 f)

onInputCapture :: (SyntheticInputEvent -> Effect Unit) -> Props
onInputCapture f = unsafeMkProps "onInputCapture" (mkEffectFn1 f)

onInvalidCapture :: (SyntheticInputEvent -> Effect Unit) -> Props
onInvalidCapture f = unsafeMkProps "onInvalidCapture" (mkEffectFn1 f)

onSubmitCapture :: (SyntheticInputEvent -> Effect Unit) -> Props
onSubmitCapture f = unsafeMkProps "onSubmitCapture" (mkEffectFn1 f)

onClickCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onClickCapture f = unsafeMkProps "onClickCapture" (mkEffectFn1 f)

onContextMenuCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onContextMenuCapture f = unsafeMkProps "onContextMenuCapture" (mkEffectFn1 f)

onDoubleClickCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDoubleClickCapture f = unsafeMkProps "onDoubleClickCapture" (mkEffectFn1 f)

onDragCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragCapture f = unsafeMkProps "onDragCapture" (mkEffectFn1 f)

onDragEndCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragEndCapture f = unsafeMkProps "onDragEndCapture" (mkEffectFn1 f)

onDragEnterCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragEnterCapture f = unsafeMkProps "onDragEnterCapture" (mkEffectFn1 f)

onDragExitCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragExitCapture f = unsafeMkProps "onDragExitCapture" (mkEffectFn1 f)

onDragLeaveCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragLeaveCapture f = unsafeMkProps "onDragLeaveCapture" (mkEffectFn1 f)

onDragOverCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragOverCapture f = unsafeMkProps "onDragOverCapture" (mkEffectFn1 f)

onDragStartCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDragStartCapture f = unsafeMkProps "onDragStartCapture" (mkEffectFn1 f)

onDropCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onDropCapture f = unsafeMkProps "onDropCapture" (mkEffectFn1 f)

onMouseDownCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseDownCapture f = unsafeMkProps "onMouseDownCapture" (mkEffectFn1 f)

onMouseEnterCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseEnterCapture f = unsafeMkProps "onMouseEnterCapture" (mkEffectFn1 f)

onMouseLeaveCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseLeaveCapture f = unsafeMkProps "onMouseLeaveCapture" (mkEffectFn1 f)

onMouseMoveCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseMoveCapture f = unsafeMkProps "onMouseMoveCapture" (mkEffectFn1 f)

onMouseOutCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseOutCapture f = unsafeMkProps "onMouseOutCapture" (mkEffectFn1 f)

onMouseOverCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseOverCapture f = unsafeMkProps "onMouseOverCapture" (mkEffectFn1 f)

onMouseUpCapture :: (SyntheticMouseEvent -> Effect Unit) -> Props
onMouseUpCapture f = unsafeMkProps "onMouseUpCapture" (mkEffectFn1 f)

onSelectCapture :: (SyntheticEvent -> Effect Unit) -> Props
onSelectCapture f = unsafeMkProps "onSelectCapture" (mkEffectFn1 f)

onTouchCancelCapture :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchCancelCapture f = unsafeMkProps "onTouchCancelCapture" (mkEffectFn1 f)

onTouchEndCapture :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchEndCapture f = unsafeMkProps "onTouchEndCapture" (mkEffectFn1 f)

onTouchMoveCapture :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchMoveCapture f = unsafeMkProps "onTouchMoveCapture" (mkEffectFn1 f)

onTouchStartCapture :: (SyntheticTouchEvent -> Effect Unit) -> Props
onTouchStartCapture f = unsafeMkProps "onTouchStartCapture" (mkEffectFn1 f)

onScrollCapture :: (SyntheticUIEvent -> Effect Unit) -> Props
onScrollCapture f = unsafeMkProps "onScrollCapture" (mkEffectFn1 f)

onWheelCapture :: (SyntheticWheelEvent -> Effect Unit) -> Props
onWheelCapture f = unsafeMkProps "onWheelCapture" (mkEffectFn1 f)

ref :: Ref.RefHandler Ref.NativeNode -> Props
ref = unsafeMkProps "ref"

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
