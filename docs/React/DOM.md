## Module React.DOM

#### `mkDOM`

``` purescript
mkDOM :: forall s dataAttrs ariaAttrs eff props state. String -> Array (DOMProps s dataAttrs ariaAttrs eff props state) -> Array UI -> UI
```

#### `text`

``` purescript
text :: String -> UI
```

#### `DOMProps`

``` purescript
data DOMProps s dataAttrs ariaAttrs eff props state
  = Accept String
  | AccessKey String
  | Action String
  | AllowFullScreen String
  | AllowTransparency String
  | Alt String
  | Aria {  | ariaAttrs }
  | Async String
  | AutoComplete String
  | AutoFocus String
  | AutoPlay String
  | CellPadding String
  | CellSpacing String
  | CharSet String
  | Checked String
  | ClassName String
  | Cols String
  | ColSpan String
  | Content String
  | ContentEditable String
  | ContextMenu String
  | Controls String
  | CrossOrigin String
  | Data {  | dataAttrs }
  | DateTime String
  | Defer String
  | Dir String
  | Disabled String
  | Download String
  | Draggable String
  | EncType String
  | Form String
  | FormNoValidate String
  | FrameBorder String
  | Height String
  | Hidden String
  | Href String
  | HrefLang String
  | HtmlFor String
  | HttpEquiv String
  | Icon String
  | Id String
  | Label String
  | Lang String
  | List String
  | Loop String
  | Max String
  | MaxLength String
  | MediaGroup String
  | Method String
  | Min String
  | Multiple String
  | Muted String
  | Name String
  | NoValidate String
  | Pattern String
  | Placeholder String
  | Poster String
  | Preload String
  | RadioGroup String
  | ReadOnly String
  | Rel String
  | Required String
  | Role String
  | Rows String
  | RowSpan String
  | Sandbox String
  | Scope String
  | ScrollLeft String
  | Scrolling String
  | ScrollTop String
  | Seamless String
  | Selected String
  | Size String
  | Span String
  | SpellCheck String
  | Src String
  | SrcDoc String
  | SrcSet String
  | Start String
  | Step String
  | Style {  | s }
  | TabIndex String
  | Target String
  | Title String
  | Type String
  | Value String
  | Width String
  | Wmode String
  | AutoCapitalize String
  | AutoCorrect String
  | Property String
  | Ref String
  | Key String
  | DangerouslySetInnerHTML { __html :: String }
  | OnBlur (EventHandler Event)
  | OnChange (EventHandler Event)
  | OnContextMenu (EventHandler Event)
  | OnCopy (EventHandler Event)
  | OnCut (EventHandler Event)
  | OnClick (EventHandler MouseEvent)
  | OnDoubleClick (EventHandler MouseEvent)
  | OnDrag (EventHandler MouseEvent)
  | OnDragEnd (EventHandler MouseEvent)
  | OnDragEnter (EventHandler MouseEvent)
  | OnDragExit (EventHandler MouseEvent)
  | OnDragLeave (EventHandler MouseEvent)
  | OnDragOver (EventHandler MouseEvent)
  | OnDragStart (EventHandler MouseEvent)
  | OnDrop (EventHandler Event)
  | OnError (EventHandler Event)
  | OnFocus (EventHandler Event)
  | OnInput (EventHandler Event)
  | OnKeyDown (EventHandler KeyboardEvent)
  | OnKeyPress (EventHandler KeyboardEvent)
  | OnKeyUp (EventHandler KeyboardEvent)
  | OnLoad (EventHandler Event)
  | OnMouseEnter (EventHandler MouseEvent)
  | OnMouseLeave (EventHandler MouseEvent)
  | OnMouseDown (EventHandler MouseEvent)
  | OnMouseMove (EventHandler MouseEvent)
  | OnMouseOut (EventHandler MouseEvent)
  | OnMouseOver (EventHandler MouseEvent)
  | OnMouseUp (EventHandler MouseEvent)
  | OnPaste (EventHandler Event)
  | OnReset (EventHandler Event)
  | OnScroll (EventHandler Event)
  | OnSubmit (EventHandler Event)
  | OnTouchCancel (EventHandler Event)
  | OnTouchEnd (EventHandler Event)
  | OnTouchMove (EventHandler Event)
  | OnTouchStart (EventHandler Event)
  | OnWheel (EventHandler Event)
```


