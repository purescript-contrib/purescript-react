module React.DOM where

  import React

  data DOMProps s dataAttrs ariaAttrs eff props state =
    Accept String
    | AccessKey String
    | Action String
    | AllowFullScreen String
    | AllowTransparency String
    | Alt String
    | Aria { | ariaAttrs }
    | Async String
    | AutoComplete String
    | AutoFocus String
    | AutoPlay String
    | CellPadding String
    | CellSpacing String
    | CharSet String
    | Checked Boolean
    | ClassName String
    | Cols String
    | ColSpan String
    | Content String
    | ContentEditable String
    | ContextMenu String
    | Controls String
    | CrossOrigin String
    | Data { | dataAttrs }
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
    | Style { | s }
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
    | DangerouslySetInnerHTML {__html :: String}

    -- events
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


  accept = Accept
  accessKey = AccessKey
  action = Action
  allowFullScreen = AllowFullScreen
  allowTransparency = AllowTransparency
  alt = Alt
  async = Async
  autoComplete = AutoComplete
  ariaSet = Aria
  autoFocus = AutoFocus
  autoPlay = AutoPlay
  cellPadding = CellPadding
  cellSpacing = CellSpacing
  charSet = CharSet
  checked = Checked
  className = ClassName
  cols = Cols
  colSpan = ColSpan
  content = Content
  contentEditable = ContentEditable
  contextMenu = ContextMenu
  controls = Controls
  crossOrigin = CrossOrigin
  dataSet = Data
  dateTime = DateTime
  defer = Defer
  dir = Dir
  disabled = Disabled
  download = Download
  draggable = Draggable
  encType = EncType
  formProp = Form
  formNoValidate = FormNoValidate
  frameBorder = FrameBorder
  height = Height
  hidden = Hidden
  href = Href
  hrefLang = HrefLang
  htmlFor = HtmlFor
  httpEquiv = HttpEquiv
  icon = Icon
  idProp = Id
  labelProp = Label
  lang = Lang
  list = List
  loop = Loop
  max = Max
  maxLength = MaxLength
  mediaGroup = MediaGroup
  method = Method
  min = Min
  multiple = Multiple
  muted = Muted
  name = Name
  noValidate = NoValidate
  pattern = Pattern
  placeholder = Placeholder
  poster = Poster
  preload = Preload
  radioGroup = RadioGroup
  readOnly = ReadOnly
  rel = Rel
  required = Required
  role = Role
  rows = Rows
  rowSpan = RowSpan
  sandbox = Sandbox
  scope = Scope
  scrollLeft = ScrollLeft
  scrolling = Scrolling
  scrollTop = ScrollTop
  seamless = Seamless
  selected = Selected
  size = Size
  spanProp = Span
  spellCheck = SpellCheck
  src = Src
  srcDoc = SrcDoc
  srcSet = SrcSet
  start = Start
  step = Step
  style = Style
  tabIndex = TabIndex
  target = Target
  titleProp = Title
  typeProp = Type
  alue = Value
  width = Width
  wmode = Wmode
  autoCapitalize = AutoCapitalize
  autoCorrect = AutoCorrect
  property = Property
  ref = Ref
  key = Key
  dangerouslySetInnerHTML v = DangerouslySetInnerHTML {__html: v}
  value = Value

  onBlur f = OnBlur $ handle f
  onChange f = OnChange $ handle f
  onClick f = OnClick $ handle f
  onContextMenu f = OnContextMenu $ handle f
  onCopy f = OnCopy $ handle f
  onCut f = OnCut $ handle f
  onDoubleClick f = OnDoubleClick $ handle f
  onDrag f = OnDrag $ handle f
  onDragEnd f = OnDragEnd $ handle f
  onDragEnter f = OnDragEnter $ handle f
  onDragExit f = OnDragExit $ handle f
  onDragLeave f = OnDragLeave $ handle f
  onDragOver f = OnDragOver $ handle f
  onDragStart f = OnDragStart $ handle f
  onDrop f = OnDrop $ handle f
  onError f = OnError $ handle f
  onFocus f = OnFocus $ handle f
  onInput f = OnInput $ handle f
  onKeyDown f = OnKeyDown $ handle f
  onKeyPress f = OnKeyPress $ handle f
  onKeyUp f = OnKeyUp $ handle f
  onLoad f = OnLoad $ handle f
  onMouseEnter f = OnMouseEnter $ handle f
  onMouseLeave f = OnMouseLeave $ handle f
  onMouseDown f = OnMouseDown $ handle f
  onMouseMove f = OnMouseMove $ handle f
  onMouseOut f = OnMouseOut $ handle f
  onMouseOver f = OnMouseOver $ handle f
  onMouseUp f = OnMouseUp $ handle f
  onPaste f = OnPaste $ handle f
  onReset f = OnReset $ handle f
  onScroll f = OnScroll $ handle f
  onSubmit f = OnSubmit $ handle f
  onTouchCancel f = OnTouchCancel $ handle f
  onTouchEnd f = OnTouchEnd $ handle f
  onTouchMove f = OnTouchMove $ handle f
  onTouchStart f = OnTouchStart $ handle f
  onWheel f = OnWheel $ handle f

  foreign import mkDOM
    " function mkProps(props) {                                \
    \   var result = {};                                       \
    \   for (var i = 0, len = props.length; i < len; i++) {    \
    \     var prop = props[i];                                 \
    \     var name = prop.ctor.substring(10);                  \
    \     name = name[0].toLowerCase() + name.substring(1);    \
    \     var val = prop.values[0];                            \
    \     /* Until React.js handles data and aria like style*/ \
    \     /* we have to unload the properties.*/               \
    \     if (name === 'data' || name === 'aria') {            \
    \       for (var subprop in val) {                         \
    \         if (val.hasOwnProperty(subprop)) {               \
    \           result[name + '-' + subprop] = val[subprop];   \
    \         }                                                \
    \       }                                                  \
    \     } else {                                             \
    \       result[name] = val;                                \
    \     }                                                    \
    \   }                                                      \
    \   return result;                                         \
    \ }                                                        \
    \ function mkDOM(tagName) {                                \
    \   var ctor = window.React.DOM[tagName];                  \
    \   return function(props) {                               \
    \     return function(children) {                          \
    \       var p = props.length > 0 ? mkProps(props) : null;  \
    \       return ctor.apply(ctor, [p].concat(children));     \
    \     }                                                    \
    \   }                                                      \
    \ }"
    :: forall s dataAttrs ariaAttrs eff props state. String -> [DOMProps s dataAttrs ariaAttrs eff props state] -> [UI] -> UI

  foreign import text
    "function text(text) { \
    \  return text;        \
    \}"
    :: String -> UI

  a = mkDOM "a"
  abbr = mkDOM "abbr"
  address = mkDOM "address"
  area = mkDOM "area"
  article = mkDOM "article"
  aside = mkDOM "aside"
  audio = mkDOM "audio"
  b = mkDOM "b"
  base = mkDOM "base"
  bdi = mkDOM "bdi"
  bdo = mkDOM "bdo"
  big = mkDOM "big"
  blockquote = mkDOM "blockquote"
  body = mkDOM "body"
  br = mkDOM "br"
  button = mkDOM "button"
  canvas = mkDOM "canvas"
  caption = mkDOM "caption"
  cite = mkDOM "cite"
  code = mkDOM "code"
  col = mkDOM "col"
  colgroup = mkDOM "colgroup"
  dd = mkDOM "dd"
  del = mkDOM "del"
  details = mkDOM "details"
  dfn = mkDOM "dfn"
  div = mkDOM "div"
  dl = mkDOM "dl"
  dt = mkDOM "dt"
  em = mkDOM "em"
  embed = mkDOM "embed"
  fieldset = mkDOM "fieldset"
  figcaption = mkDOM "figcaption"
  figure = mkDOM "figure"
  footer = mkDOM "footer"
  form = mkDOM "form"
  h1 = mkDOM "h1"
  h2 = mkDOM "h2"
  h3 = mkDOM "h3"
  h4 = mkDOM "h4"
  h5 = mkDOM "h5"
  h6 = mkDOM "h6"
  headDOM = mkDOM "head"
  header = mkDOM "header"
  hr = mkDOM "hr"
  html = mkDOM "html"
  i = mkDOM "i"
  iframe = mkDOM "iframe"
  iframe = mkDOM "iframe"
  img = mkDOM "img"
  input = mkDOM "input"
  ins = mkDOM "ins"
  kbd = mkDOM "kbd"
  keygen = mkDOM "keygen"
  label = mkDOM "label"
  legend = mkDOM "legend"
  li = mkDOM "li"
  link = mkDOM "link"
  mainDOM = mkDOM "main"
  mapDOM = mkDOM "map"
  mark = mkDOM "mark"
  menu = mkDOM "menu"
  menuitem = mkDOM "menuitem"
  meta = mkDOM "meta"
  meter = mkDOM "meter"
  nav = mkDOM "nav"
  noscript = mkDOM "noscript"
  object = mkDOM "object"
  ol = mkDOM "ol"
  optgroup = mkDOM "optgroup"
  option = mkDOM "option"
  output = mkDOM "output"
  p = mkDOM "p"
  param = mkDOM "param"
  param = mkDOM "param"
  pre = mkDOM "pre"
  progress = mkDOM "progress"
  q = mkDOM "q"
  rp = mkDOM "rp"
  rp = mkDOM "rp"
  rt = mkDOM "rt"
  ruby = mkDOM "ruby"
  s = mkDOM "s"
  samp = mkDOM "samp"
  samp = mkDOM "samp"
  script = mkDOM "script"
  section = mkDOM "section"
  select = mkDOM "select"
  small = mkDOM "small"
  source = mkDOM "source"
  span = mkDOM "span"
  strong = mkDOM "strong"
  styleDOM = mkDOM "style"
  sub = mkDOM "sub"
  summary = mkDOM "summary"
  sup = mkDOM "sup"
  table = mkDOM "table"
  tbody = mkDOM "tbody"
  td = mkDOM "td"
  textarea = mkDOM "textarea"
  tfoot = mkDOM "tfoot"
  th = mkDOM "th"
  thead = mkDOM "thead"
  time = mkDOM "time"
  title = mkDOM "title"
  tr = mkDOM "tr"
  track = mkDOM "track"
  u = mkDOM "u"
  ul = mkDOM "ul"
  ul = mkDOM "ul"
  var = mkDOM "var"
  video = mkDOM "video"
  wbr = mkDOM "wbr"
  circle = mkDOM "circle"
  defs = mkDOM "defs"
  g = mkDOM "g"
  line = mkDOM "line"
  line = mkDOM "line"
  linearGradient = mkDOM "linearGradient"
  path = mkDOM "path"
  polygon = mkDOM "polygon"
  polyline = mkDOM "polyline"
  radialGradient = mkDOM "radialGradient"
  rect = mkDOM "rect"
  stop = mkDOM "stop"
  svg = mkDOM "svg"

  -- | Props-less versions
  a' = a []
  abbr' = abbr []
  address' = address []
  area' = area []
  article' = article []
  aside' = aside []
  audio' = audio []
  b' = b []
  base' = base []
  base' = base []
  bdi' = bdi []
  bdo' = bdo []
  big' = big []
  blockquote' = blockquote []
  body' = body []
  br' = br []
  button' = button []
  canvas' = canvas []
  caption' = caption []
  cite' = cite []
  code' = code []
  col' = col []
  colgroup' = colgroup []
  dd' = dd []
  del' = del []
  details' = details []
  dfn' = dfn []
  div' = div []
  dl' = dl []
  dt' = dt []
  em' = em []
  embed' = embed []
  fieldset' = fieldset []
  figcaption' = figcaption []
  figure' = figure []
  footer' = footer []
  form' = form []
  h1' = h1 []
  h2' = h2 []
  h3' = h3 []
  h4' = h4 []
  h5' = h5 []
  h6' = h6 []
  headDOM' = headDOM []
  header' = header []
  hr' = hr []
  html' = html []
  i' = i []
  iframe' = iframe []
  iframe' = iframe []
  img' = img []
  input' = input []
  ins' = ins []
  kbd' = kbd []
  keygen' = keygen []
  label' = label []
  legend' = legend []
  li' = li []
  link' = link []
  mainDOM' = mainDOM []
  mapDOM' = mapDOM []
  mark' = mark []
  menu' = menu []
  menuitem' = menuitem []
  meta' = meta []
  meter' = meter []
  nav' = nav []
  noscript' = noscript []
  object' = object []
  ol' = ol []
  optgroup' = optgroup []
  option' = option []
  output' = output []
  p' = p []
  param' = param []
  param' = param []
  pre' = pre []
  progress' = progress []
  q' = q []
  rp' = rp []
  rp' = rp []
  rt' = rt []
  ruby' = ruby []
  s' = s []
  samp' = samp []
  samp' = samp []
  script' = script []
  section' = section []
  select' = select []
  small' = small []
  source' = source []
  span' = span []
  strong' = strong []
  styleDOM' = styleDOM []
  sub' = sub []
  summary' = summary []
  sup' = sup []
  table' = table []
  tbody' = tbody []
  td' = td []
  textarea' = textarea []
  tfoot' = tfoot []
  th' = th []
  thead' = thead []
  time' = time []
  title' = title []
  tr' = tr []
  track' = track []
  u' = u []
  ul' = ul []
  ul' = ul []
  var' = var []
  video' = video []
  wbr' = wbr []
  circle' = circle []
  defs' = defs []
  g' = g []
  line' = line []
  line' = line []
  linearGradient' = linearGradient []
  path' = path []
  polygon' = polygon []
  polyline' = polyline []
  radialGradient' = radialGradient []
  rect' = rect []
  stop' = stop []
  svg' = svg []
