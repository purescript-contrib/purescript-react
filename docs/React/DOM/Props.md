## Module React.DOM.Props

#### `Props`

``` purescript
data Props :: *
```

#### `unsafeMkProps`

``` purescript
unsafeMkProps :: forall val. String -> val -> Props
```

#### `unsafeUnfoldProps`

``` purescript
unsafeUnfoldProps :: forall vals. String -> {  | vals } -> Props
```

#### `aria`

``` purescript
aria :: forall ariaAttrs. {  | ariaAttrs } -> Props
```

#### `_data`

``` purescript
_data :: forall dataAttrs. {  | dataAttrs } -> Props
```

#### `style`

``` purescript
style :: forall style. {  | style } -> Props
```

#### `dangerouslySetInnerHTML`

``` purescript
dangerouslySetInnerHTML :: { __html :: String } -> Props
```

#### `accept`

``` purescript
accept :: String -> Props
```

#### `acceptCharset`

``` purescript
acceptCharset :: String -> Props
```

#### `accessKey`

``` purescript
accessKey :: String -> Props
```

#### `action`

``` purescript
action :: String -> Props
```

#### `allowFullScreen`

``` purescript
allowFullScreen :: String -> Props
```

#### `allowTransparency`

``` purescript
allowTransparency :: String -> Props
```

#### `alt`

``` purescript
alt :: String -> Props
```

#### `async`

``` purescript
async :: String -> Props
```

#### `autoComplete`

``` purescript
autoComplete :: String -> Props
```

#### `autoFocus`

``` purescript
autoFocus :: Boolean -> Props
```

#### `autoPlay`

``` purescript
autoPlay :: String -> Props
```

#### `cellPadding`

``` purescript
cellPadding :: String -> Props
```

#### `cellSpacing`

``` purescript
cellSpacing :: String -> Props
```

#### `charSet`

``` purescript
charSet :: String -> Props
```

#### `checked`

``` purescript
checked :: String -> Props
```

#### `classID`

``` purescript
classID :: String -> Props
```

#### `className`

``` purescript
className :: String -> Props
```

#### `cols`

``` purescript
cols :: String -> Props
```

#### `colSpan`

``` purescript
colSpan :: String -> Props
```

#### `content`

``` purescript
content :: String -> Props
```

#### `contentEditable`

``` purescript
contentEditable :: String -> Props
```

#### `contextMenu`

``` purescript
contextMenu :: String -> Props
```

#### `controls`

``` purescript
controls :: String -> Props
```

#### `coords`

``` purescript
coords :: String -> Props
```

#### `crossOrigin`

``` purescript
crossOrigin :: String -> Props
```

#### `dateTime`

``` purescript
dateTime :: String -> Props
```

#### `defer`

``` purescript
defer :: String -> Props
```

#### `dir`

``` purescript
dir :: String -> Props
```

#### `disabled`

``` purescript
disabled :: Boolean -> Props
```

#### `download`

``` purescript
download :: String -> Props
```

#### `draggable`

``` purescript
draggable :: String -> Props
```

#### `encType`

``` purescript
encType :: String -> Props
```

#### `form`

``` purescript
form :: String -> Props
```

#### `formAction`

``` purescript
formAction :: String -> Props
```

#### `formEncType`

``` purescript
formEncType :: String -> Props
```

#### `formMethod`

``` purescript
formMethod :: String -> Props
```

#### `formNoValidate`

``` purescript
formNoValidate :: String -> Props
```

#### `formTarget`

``` purescript
formTarget :: String -> Props
```

#### `frameBorder`

``` purescript
frameBorder :: String -> Props
```

#### `height`

``` purescript
height :: String -> Props
```

#### `hidden`

``` purescript
hidden :: String -> Props
```

#### `href`

``` purescript
href :: String -> Props
```

#### `hrefLang`

``` purescript
hrefLang :: String -> Props
```

#### `htmlFor`

``` purescript
htmlFor :: String -> Props
```

#### `httpEquiv`

``` purescript
httpEquiv :: String -> Props
```

#### `icon`

``` purescript
icon :: String -> Props
```

#### `_id`

``` purescript
_id :: String -> Props
```

#### `key`

``` purescript
key :: String -> Props
```

#### `label`

``` purescript
label :: String -> Props
```

#### `lang`

``` purescript
lang :: String -> Props
```

#### `list`

``` purescript
list :: String -> Props
```

#### `loop`

``` purescript
loop :: String -> Props
```

#### `manifest`

``` purescript
manifest :: String -> Props
```

#### `marginHeight`

``` purescript
marginHeight :: String -> Props
```

#### `marginWidth`

``` purescript
marginWidth :: String -> Props
```

#### `max`

``` purescript
max :: String -> Props
```

#### `maxLength`

``` purescript
maxLength :: String -> Props
```

#### `media`

``` purescript
media :: String -> Props
```

#### `mediaGroup`

``` purescript
mediaGroup :: String -> Props
```

#### `method`

``` purescript
method :: String -> Props
```

#### `min`

``` purescript
min :: String -> Props
```

#### `multiple`

``` purescript
multiple :: String -> Props
```

#### `muted`

``` purescript
muted :: String -> Props
```

#### `name`

``` purescript
name :: String -> Props
```

#### `noValidate`

``` purescript
noValidate :: String -> Props
```

#### `open`

``` purescript
open :: String -> Props
```

#### `pattern`

``` purescript
pattern :: String -> Props
```

#### `placeholder`

``` purescript
placeholder :: String -> Props
```

#### `poster`

``` purescript
poster :: String -> Props
```

#### `preload`

``` purescript
preload :: String -> Props
```

#### `radioGroup`

``` purescript
radioGroup :: String -> Props
```

#### `readOnly`

``` purescript
readOnly :: String -> Props
```

#### `rel`

``` purescript
rel :: String -> Props
```

#### `required`

``` purescript
required :: String -> Props
```

#### `role`

``` purescript
role :: String -> Props
```

#### `rows`

``` purescript
rows :: String -> Props
```

#### `rowSpan`

``` purescript
rowSpan :: String -> Props
```

#### `sandbox`

``` purescript
sandbox :: String -> Props
```

#### `scope`

``` purescript
scope :: String -> Props
```

#### `scrolling`

``` purescript
scrolling :: String -> Props
```

#### `seamless`

``` purescript
seamless :: String -> Props
```

#### `selected`

``` purescript
selected :: String -> Props
```

#### `shape`

``` purescript
shape :: String -> Props
```

#### `size`

``` purescript
size :: String -> Props
```

#### `sizes`

``` purescript
sizes :: String -> Props
```

#### `span`

``` purescript
span :: String -> Props
```

#### `spellCheck`

``` purescript
spellCheck :: String -> Props
```

#### `src`

``` purescript
src :: String -> Props
```

#### `srcDoc`

``` purescript
srcDoc :: String -> Props
```

#### `srcSet`

``` purescript
srcSet :: String -> Props
```

#### `start`

``` purescript
start :: String -> Props
```

#### `step`

``` purescript
step :: String -> Props
```

#### `tabIndex`

``` purescript
tabIndex :: String -> Props
```

#### `target`

``` purescript
target :: String -> Props
```

#### `title`

``` purescript
title :: String -> Props
```

#### `_type`

``` purescript
_type :: String -> Props
```

#### `useMap`

``` purescript
useMap :: String -> Props
```

#### `value`

``` purescript
value :: String -> Props
```

#### `width`

``` purescript
width :: String -> Props
```

#### `wmode`

``` purescript
wmode :: String -> Props
```

#### `onCopy`

``` purescript
onCopy :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onCut`

``` purescript
onCut :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onPaste`

``` purescript
onPaste :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall eff props state result. (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall eff props state result. (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall eff props state result. (KeyboardEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onFocus`

``` purescript
onFocus :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onBlur`

``` purescript
onBlur :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onChange`

``` purescript
onChange :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onInput`

``` purescript
onInput :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onSubmit`

``` purescript
onSubmit :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onClick`

``` purescript
onClick :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDrag`

``` purescript
onDrag :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDragEnd`

``` purescript
onDragEnd :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDragEnter`

``` purescript
onDragEnter :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDragExit`

``` purescript
onDragExit :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDragLeave`

``` purescript
onDragLeave :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDragOver`

``` purescript
onDragOver :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDragStart`

``` purescript
onDragStart :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onDrop`

``` purescript
onDrop :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall eff props state result. (MouseEvent -> EventHandlerContext eff props state result) -> Props
```

#### `onTouchCancel`

``` purescript
onTouchCancel :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onTouchEnd`

``` purescript
onTouchEnd :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onTouchMove`

``` purescript
onTouchMove :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onTouchStart`

``` purescript
onTouchStart :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onScroll`

``` purescript
onScroll :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```

#### `onWheel`

``` purescript
onWheel :: forall eff props state result. (Event -> EventHandlerContext eff props state result) -> Props
```


