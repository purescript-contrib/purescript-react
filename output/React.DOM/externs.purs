module React.DOM (DOMProps(Accept, AccessKey, Action, AllowFullScreen, AllowTransparency, Alt, Aria, Async, AutoComplete, AutoFocus, AutoPlay, CellPadding, CellSpacing, CharSet, Checked, ClassName, Cols, ColSpan, Content, ContentEditable, ContextMenu, Controls, CrossOrigin, Data, DateTime, Defer, Dir, Disabled, Download, Draggable, EncType, Form, FormNoValidate, FrameBorder, Height, Hidden, Href, HrefLang, HtmlFor, HttpEquiv, Icon, Id, Label, Lang, List, Loop, Max, MaxLength, MediaGroup, Method, Min, Multiple, Muted, Name, NoValidate, Pattern, Placeholder, Poster, Preload, RadioGroup, ReadOnly, Rel, Required, Role, Rows, RowSpan, Sandbox, Scope, ScrollLeft, Scrolling, ScrollTop, Seamless, Selected, Size, Span, SpellCheck, Src, SrcDoc, SrcSet, Start, Step, Style, TabIndex, Target, Title, Type, Value, Width, Wmode, AutoCapitalize, AutoCorrect, Property, Ref, Key, DangerouslySetInnerHTML, OnBlur, OnChange, OnContextMenu, OnCopy, OnCut, OnClick, OnDoubleClick, OnDrag, OnDragEnd, OnDragEnter, OnDragExit, OnDragLeave, OnDragOver, OnDragStart, OnDrop, OnError, OnFocus, OnInput, OnKeyDown, OnKeyPress, OnKeyUp, OnLoad, OnMouseEnter, OnMouseLeave, OnMouseDown, OnMouseMove, OnMouseOut, OnMouseOver, OnMouseUp, OnPaste, OnReset, OnScroll, OnSubmit, OnTouchCancel, OnTouchEnd, OnTouchMove, OnTouchStart, OnWheel), svg', stop', rect', radialGradient', polyline', polygon', path', linearGradient', line', g', defs', circle', wbr', video', var', ul', u', track', tr', title', time', thead', th', tfoot', textarea', td', tbody', table', sup', summary', sub', styleDOM', strong', span', source', small', select', section', script', samp', s', ruby', rt', rp', q', progress', pre', param', p', output', option', optgroup', ol', object', noscript', nav', meter', meta', menuitem', menu', mark', mapDOM', mainDOM', link', li', legend', label', keygen', kbd', ins', input', img', iframe', i', html', hr', header', headDOM', h6', h5', h4', h3', h2', h1', form', footer', figure', figcaption', fieldset', embed', em', dt', dl', div', dfn', details', del', dd', colgroup', col', code', cite', caption', canvas', button', br', body', blockquote', big', bdo', bdi', base', b', audio', aside', article', area', address', abbr', a', svg, stop, rect, radialGradient, polyline, polygon, path, linearGradient, line, g, defs, circle, wbr, video, var, ul, u, track, tr, title, time, thead, th, tfoot, textarea, td, tbody, table, sup, summary, sub, styleDOM, strong, span, source, small, select, section, script, samp, s, ruby, rt, rp, q, progress, pre, param, p, output, option, optgroup, ol, object, noscript, nav, meter, meta, menuitem, menu, mark, mapDOM, mainDOM, link, li, legend, label, keygen, kbd, ins, input, img, iframe, i, html, hr, header, headDOM, h6, h5, h4, h3, h2, h1, form, footer, figure, figcaption, fieldset, embed, em, dt, dl, div, dfn, details, del, dd, colgroup, col, code, cite, caption, canvas, button, br, body, blockquote, big, bdo, bdi, base, b, audio, aside, article, area, address, abbr, a, onWheel, onTouchStart, onTouchMove, onTouchEnd, onTouchCancel, onSubmit, onScroll, onReset, onPaste, onMouseUp, onMouseOver, onMouseOut, onMouseMove, onMouseDown, onMouseLeave, onMouseEnter, onLoad, onKeyUp, onKeyPress, onKeyDown, onInput, onFocus, onError, onDrop, onDragStart, onDragOver, onDragLeave, onDragExit, onDragEnter, onDragEnd, onDrag, onDoubleClick, onCut, onCopy, onContextMenu, onClick, onChange, onBlur, value, dangerouslySetInnerHTML, key, ref, property, autoCorrect, autoCapitalize, wmode, width, alue, typeProp, titleProp, target, tabIndex, style, step, start, srcSet, srcDoc, src, spellCheck, spanProp, size, selected, seamless, scrollTop, scrolling, scrollLeft, scope, sandbox, rowSpan, rows, role, required, rel, readOnly, radioGroup, preload, poster, placeholder, pattern, noValidate, name, muted, multiple, min, method, mediaGroup, maxLength, max, loop, list, lang, labelProp, idProp, icon, httpEquiv, htmlFor, hrefLang, href, hidden, height, frameBorder, formNoValidate, formProp, encType, draggable, download, disabled, dir, defer, dateTime, dataSet, crossOrigin, controls, contextMenu, contentEditable, content, colSpan, cols, className, checked, charSet, cellSpacing, cellPadding, autoPlay, autoFocus, ariaSet, autoComplete, async, alt, allowTransparency, allowFullScreen, action, accessKey, accept, text, mkDOM) where
import Prelude ()
import React ()
import React.DOM ()
import Prim ()
import Prelude ()
import React ()
--  | Props-less versions
data DOMProps (s :: # *) (dataAttrs :: # *) (ariaAttrs :: # *) (eff :: *) (props :: *) (state :: *) = Accept Prim.String | AccessKey Prim.String | Action Prim.String | AllowFullScreen Prim.String | AllowTransparency Prim.String | Alt Prim.String | Aria {  | ariaAttrs } | Async Prim.String | AutoComplete Prim.String | AutoFocus Prim.String | AutoPlay Prim.String | CellPadding Prim.String | CellSpacing Prim.String | CharSet Prim.String | Checked Prim.String | ClassName Prim.String | Cols Prim.String | ColSpan Prim.String | Content Prim.String | ContentEditable Prim.String | ContextMenu Prim.String | Controls Prim.String | CrossOrigin Prim.String | Data {  | dataAttrs } | DateTime Prim.String | Defer Prim.String | Dir Prim.String | Disabled Prim.String | Download Prim.String | Draggable Prim.String | EncType Prim.String | Form Prim.String | FormNoValidate Prim.String | FrameBorder Prim.String | Height Prim.String | Hidden Prim.String | Href Prim.String | HrefLang Prim.String | HtmlFor Prim.String | HttpEquiv Prim.String | Icon Prim.String | Id Prim.String | Label Prim.String | Lang Prim.String | List Prim.String | Loop Prim.String | Max Prim.String | MaxLength Prim.String | MediaGroup Prim.String | Method Prim.String | Min Prim.String | Multiple Prim.String | Muted Prim.String | Name Prim.String | NoValidate Prim.String | Pattern Prim.String | Placeholder Prim.String | Poster Prim.String | Preload Prim.String | RadioGroup Prim.String | ReadOnly Prim.String | Rel Prim.String | Required Prim.String | Role Prim.String | Rows Prim.String | RowSpan Prim.String | Sandbox Prim.String | Scope Prim.String | ScrollLeft Prim.String | Scrolling Prim.String | ScrollTop Prim.String | Seamless Prim.String | Selected Prim.String | Size Prim.String | Span Prim.String | SpellCheck Prim.String | Src Prim.String | SrcDoc Prim.String | SrcSet Prim.String | Start Prim.String | Step Prim.String | Style {  | s } | TabIndex Prim.String | Target Prim.String | Title Prim.String | Type Prim.String | Value Prim.String | Width Prim.String | Wmode Prim.String | AutoCapitalize Prim.String | AutoCorrect Prim.String | Property Prim.String | Ref Prim.String | Key Prim.String | DangerouslySetInnerHTML { __html :: Prim.String } | OnBlur (React.EventHandler React.Event) | OnChange (React.EventHandler React.Event) | OnContextMenu (React.EventHandler React.Event) | OnCopy (React.EventHandler React.Event) | OnCut (React.EventHandler React.Event) | OnClick (React.EventHandler React.MouseEvent) | OnDoubleClick (React.EventHandler React.MouseEvent) | OnDrag (React.EventHandler React.MouseEvent) | OnDragEnd (React.EventHandler React.MouseEvent) | OnDragEnter (React.EventHandler React.MouseEvent) | OnDragExit (React.EventHandler React.MouseEvent) | OnDragLeave (React.EventHandler React.MouseEvent) | OnDragOver (React.EventHandler React.MouseEvent) | OnDragStart (React.EventHandler React.MouseEvent) | OnDrop (React.EventHandler React.Event) | OnError (React.EventHandler React.Event) | OnFocus (React.EventHandler React.Event) | OnInput (React.EventHandler React.Event) | OnKeyDown (React.EventHandler React.KeyboardEvent) | OnKeyPress (React.EventHandler React.KeyboardEvent) | OnKeyUp (React.EventHandler React.KeyboardEvent) | OnLoad (React.EventHandler React.Event) | OnMouseEnter (React.EventHandler React.MouseEvent) | OnMouseLeave (React.EventHandler React.MouseEvent) | OnMouseDown (React.EventHandler React.MouseEvent) | OnMouseMove (React.EventHandler React.MouseEvent) | OnMouseOut (React.EventHandler React.MouseEvent) | OnMouseOver (React.EventHandler React.MouseEvent) | OnMouseUp (React.EventHandler React.MouseEvent) | OnPaste (React.EventHandler React.Event) | OnReset (React.EventHandler React.Event) | OnScroll (React.EventHandler React.Event) | OnSubmit (React.EventHandler React.Event) | OnTouchCancel (React.EventHandler React.Event) | OnTouchEnd (React.EventHandler React.Event) | OnTouchMove (React.EventHandler React.Event) | OnTouchStart (React.EventHandler React.Event) | OnWheel (React.EventHandler React.Event)
foreign import svg' :: Prim.Array React.UI -> React.UI
foreign import stop' :: Prim.Array React.UI -> React.UI
foreign import rect' :: Prim.Array React.UI -> React.UI
foreign import radialGradient' :: Prim.Array React.UI -> React.UI
foreign import polyline' :: Prim.Array React.UI -> React.UI
foreign import polygon' :: Prim.Array React.UI -> React.UI
foreign import path' :: Prim.Array React.UI -> React.UI
foreign import linearGradient' :: Prim.Array React.UI -> React.UI
foreign import line' :: Prim.Array React.UI -> React.UI
foreign import g' :: Prim.Array React.UI -> React.UI
foreign import defs' :: Prim.Array React.UI -> React.UI
foreign import circle' :: Prim.Array React.UI -> React.UI
foreign import wbr' :: Prim.Array React.UI -> React.UI
foreign import video' :: Prim.Array React.UI -> React.UI
foreign import var' :: Prim.Array React.UI -> React.UI
foreign import ul' :: Prim.Array React.UI -> React.UI
foreign import u' :: Prim.Array React.UI -> React.UI
foreign import track' :: Prim.Array React.UI -> React.UI
foreign import tr' :: Prim.Array React.UI -> React.UI
foreign import title' :: Prim.Array React.UI -> React.UI
foreign import time' :: Prim.Array React.UI -> React.UI
foreign import thead' :: Prim.Array React.UI -> React.UI
foreign import th' :: Prim.Array React.UI -> React.UI
foreign import tfoot' :: Prim.Array React.UI -> React.UI
foreign import textarea' :: Prim.Array React.UI -> React.UI
foreign import td' :: Prim.Array React.UI -> React.UI
foreign import tbody' :: Prim.Array React.UI -> React.UI
foreign import table' :: Prim.Array React.UI -> React.UI
foreign import sup' :: Prim.Array React.UI -> React.UI
foreign import summary' :: Prim.Array React.UI -> React.UI
foreign import sub' :: Prim.Array React.UI -> React.UI
foreign import styleDOM' :: Prim.Array React.UI -> React.UI
foreign import strong' :: Prim.Array React.UI -> React.UI
foreign import span' :: Prim.Array React.UI -> React.UI
foreign import source' :: Prim.Array React.UI -> React.UI
foreign import small' :: Prim.Array React.UI -> React.UI
foreign import select' :: Prim.Array React.UI -> React.UI
foreign import section' :: Prim.Array React.UI -> React.UI
foreign import script' :: Prim.Array React.UI -> React.UI
foreign import samp' :: Prim.Array React.UI -> React.UI
foreign import s' :: Prim.Array React.UI -> React.UI
foreign import ruby' :: Prim.Array React.UI -> React.UI
foreign import rt' :: Prim.Array React.UI -> React.UI
foreign import rp' :: Prim.Array React.UI -> React.UI
foreign import q' :: Prim.Array React.UI -> React.UI
foreign import progress' :: Prim.Array React.UI -> React.UI
foreign import pre' :: Prim.Array React.UI -> React.UI
foreign import param' :: Prim.Array React.UI -> React.UI
foreign import p' :: Prim.Array React.UI -> React.UI
foreign import output' :: Prim.Array React.UI -> React.UI
foreign import option' :: Prim.Array React.UI -> React.UI
foreign import optgroup' :: Prim.Array React.UI -> React.UI
foreign import ol' :: Prim.Array React.UI -> React.UI
foreign import object' :: Prim.Array React.UI -> React.UI
foreign import noscript' :: Prim.Array React.UI -> React.UI
foreign import nav' :: Prim.Array React.UI -> React.UI
foreign import meter' :: Prim.Array React.UI -> React.UI
foreign import meta' :: Prim.Array React.UI -> React.UI
foreign import menuitem' :: Prim.Array React.UI -> React.UI
foreign import menu' :: Prim.Array React.UI -> React.UI
foreign import mark' :: Prim.Array React.UI -> React.UI
foreign import mapDOM' :: Prim.Array React.UI -> React.UI
foreign import mainDOM' :: Prim.Array React.UI -> React.UI
foreign import link' :: Prim.Array React.UI -> React.UI
foreign import li' :: Prim.Array React.UI -> React.UI
foreign import legend' :: Prim.Array React.UI -> React.UI
foreign import label' :: Prim.Array React.UI -> React.UI
foreign import keygen' :: Prim.Array React.UI -> React.UI
foreign import kbd' :: Prim.Array React.UI -> React.UI
foreign import ins' :: Prim.Array React.UI -> React.UI
foreign import input' :: Prim.Array React.UI -> React.UI
foreign import img' :: Prim.Array React.UI -> React.UI
foreign import iframe' :: Prim.Array React.UI -> React.UI
foreign import i' :: Prim.Array React.UI -> React.UI
foreign import html' :: Prim.Array React.UI -> React.UI
foreign import hr' :: Prim.Array React.UI -> React.UI
foreign import header' :: Prim.Array React.UI -> React.UI
foreign import headDOM' :: Prim.Array React.UI -> React.UI
foreign import h6' :: Prim.Array React.UI -> React.UI
foreign import h5' :: Prim.Array React.UI -> React.UI
foreign import h4' :: Prim.Array React.UI -> React.UI
foreign import h3' :: Prim.Array React.UI -> React.UI
foreign import h2' :: Prim.Array React.UI -> React.UI
foreign import h1' :: Prim.Array React.UI -> React.UI
foreign import form' :: Prim.Array React.UI -> React.UI
foreign import footer' :: Prim.Array React.UI -> React.UI
foreign import figure' :: Prim.Array React.UI -> React.UI
foreign import figcaption' :: Prim.Array React.UI -> React.UI
foreign import fieldset' :: Prim.Array React.UI -> React.UI
foreign import embed' :: Prim.Array React.UI -> React.UI
foreign import em' :: Prim.Array React.UI -> React.UI
foreign import dt' :: Prim.Array React.UI -> React.UI
foreign import dl' :: Prim.Array React.UI -> React.UI
foreign import div' :: Prim.Array React.UI -> React.UI
foreign import dfn' :: Prim.Array React.UI -> React.UI
foreign import details' :: Prim.Array React.UI -> React.UI
foreign import del' :: Prim.Array React.UI -> React.UI
foreign import dd' :: Prim.Array React.UI -> React.UI
foreign import colgroup' :: Prim.Array React.UI -> React.UI
foreign import col' :: Prim.Array React.UI -> React.UI
foreign import code' :: Prim.Array React.UI -> React.UI
foreign import cite' :: Prim.Array React.UI -> React.UI
foreign import caption' :: Prim.Array React.UI -> React.UI
foreign import canvas' :: Prim.Array React.UI -> React.UI
foreign import button' :: Prim.Array React.UI -> React.UI
foreign import br' :: Prim.Array React.UI -> React.UI
foreign import body' :: Prim.Array React.UI -> React.UI
foreign import blockquote' :: Prim.Array React.UI -> React.UI
foreign import big' :: Prim.Array React.UI -> React.UI
foreign import bdo' :: Prim.Array React.UI -> React.UI
foreign import bdi' :: Prim.Array React.UI -> React.UI
foreign import base' :: Prim.Array React.UI -> React.UI
foreign import b' :: Prim.Array React.UI -> React.UI
foreign import audio' :: Prim.Array React.UI -> React.UI
foreign import aside' :: Prim.Array React.UI -> React.UI
foreign import article' :: Prim.Array React.UI -> React.UI
foreign import area' :: Prim.Array React.UI -> React.UI
foreign import address' :: Prim.Array React.UI -> React.UI
foreign import abbr' :: Prim.Array React.UI -> React.UI
foreign import a' :: Prim.Array React.UI -> React.UI
foreign import svg :: forall t544 t545 t546 t547 t548 t549. Prim.Array (React.DOM.DOMProps t549 t548 t547 t546 t545 t544) -> Prim.Array React.UI -> React.UI
foreign import stop :: forall t707 t708 t709 t710 t711 t712. Prim.Array (React.DOM.DOMProps t712 t711 t710 t709 t708 t707) -> Prim.Array React.UI -> React.UI
foreign import rect :: forall t1166 t1167 t1168 t1169 t1170 t1171. Prim.Array (React.DOM.DOMProps t1171 t1170 t1169 t1168 t1167 t1166) -> Prim.Array React.UI -> React.UI
foreign import radialGradient :: forall t1206 t1207 t1208 t1209 t1210 t1211. Prim.Array (React.DOM.DOMProps t1211 t1210 t1209 t1208 t1207 t1206) -> Prim.Array React.UI -> React.UI
foreign import polyline :: forall t1331 t1332 t1333 t1334 t1335 t1336. Prim.Array (React.DOM.DOMProps t1336 t1335 t1334 t1333 t1332 t1331) -> Prim.Array React.UI -> React.UI
foreign import polygon :: forall t1357 t1358 t1359 t1360 t1361 t1362. Prim.Array (React.DOM.DOMProps t1362 t1361 t1360 t1359 t1358 t1357) -> Prim.Array React.UI -> React.UI
foreign import path :: forall t1397 t1398 t1399 t1400 t1401 t1402. Prim.Array (React.DOM.DOMProps t1402 t1401 t1400 t1399 t1398 t1397) -> Prim.Array React.UI -> React.UI
foreign import linearGradient :: forall t3082 t3083 t3084 t3085 t3086 t3087. Prim.Array (React.DOM.DOMProps t3087 t3086 t3085 t3084 t3083 t3082) -> Prim.Array React.UI -> React.UI
foreign import line :: forall t3108 t3109 t3110 t3111 t3112 t3113. Prim.Array (React.DOM.DOMProps t3113 t3112 t3111 t3110 t3109 t3108) -> Prim.Array React.UI -> React.UI
foreign import g :: forall t3731 t3732 t3733 t3734 t3735 t3736. Prim.Array (React.DOM.DOMProps t3736 t3735 t3734 t3733 t3732 t3731) -> Prim.Array React.UI -> React.UI
foreign import defs :: forall t4151 t4152 t4153 t4154 t4155 t4156. Prim.Array (React.DOM.DOMProps t4156 t4155 t4154 t4153 t4152 t4151) -> Prim.Array React.UI -> React.UI
foreign import circle :: forall t4392 t4393 t4394 t4395 t4396 t4397. Prim.Array (React.DOM.DOMProps t4397 t4396 t4395 t4394 t4393 t4392) -> Prim.Array React.UI -> React.UI
foreign import wbr :: forall t93 t94 t95 t96 t97 t98. Prim.Array (React.DOM.DOMProps t98 t97 t96 t95 t94 t93) -> Prim.Array React.UI -> React.UI
foreign import video :: forall t119 t120 t121 t122 t123 t124. Prim.Array (React.DOM.DOMProps t124 t123 t122 t121 t120 t119) -> Prim.Array React.UI -> React.UI
foreign import var :: forall t145 t146 t147 t148 t149 t150. Prim.Array (React.DOM.DOMProps t150 t149 t148 t147 t146 t145) -> Prim.Array React.UI -> React.UI
foreign import ul :: forall t178 t179 t180 t181 t182 t183. Prim.Array (React.DOM.DOMProps t183 t182 t181 t180 t179 t178) -> Prim.Array React.UI -> React.UI
foreign import u :: forall t204 t205 t206 t207 t208 t209. Prim.Array (React.DOM.DOMProps t209 t208 t207 t206 t205 t204) -> Prim.Array React.UI -> React.UI
foreign import track :: forall t237 t238 t239 t240 t241 t242. Prim.Array (React.DOM.DOMProps t242 t241 t240 t239 t238 t237) -> Prim.Array React.UI -> React.UI
foreign import tr :: forall t263 t264 t265 t266 t267 t268. Prim.Array (React.DOM.DOMProps t268 t267 t266 t265 t264 t263) -> Prim.Array React.UI -> React.UI
foreign import title :: forall t296 t297 t298 t299 t300 t301. Prim.Array (React.DOM.DOMProps t301 t300 t299 t298 t297 t296) -> Prim.Array React.UI -> React.UI
foreign import time :: forall t322 t323 t324 t325 t326 t327. Prim.Array (React.DOM.DOMProps t327 t326 t325 t324 t323 t322) -> Prim.Array React.UI -> React.UI
foreign import thead :: forall t348 t349 t350 t351 t352 t353. Prim.Array (React.DOM.DOMProps t353 t352 t351 t350 t349 t348) -> Prim.Array React.UI -> React.UI
foreign import th :: forall t374 t375 t376 t377 t378 t379. Prim.Array (React.DOM.DOMProps t379 t378 t377 t376 t375 t374) -> Prim.Array React.UI -> React.UI
foreign import tfoot :: forall t400 t401 t402 t403 t404 t405. Prim.Array (React.DOM.DOMProps t405 t404 t403 t402 t401 t400) -> Prim.Array React.UI -> React.UI
foreign import textarea :: forall t426 t427 t428 t429 t430 t431. Prim.Array (React.DOM.DOMProps t431 t430 t429 t428 t427 t426) -> Prim.Array React.UI -> React.UI
foreign import td :: forall t452 t453 t454 t455 t456 t457. Prim.Array (React.DOM.DOMProps t457 t456 t455 t454 t453 t452) -> Prim.Array React.UI -> React.UI
foreign import tbody :: forall t478 t479 t480 t481 t482 t483. Prim.Array (React.DOM.DOMProps t483 t482 t481 t480 t479 t478) -> Prim.Array React.UI -> React.UI
foreign import table :: forall t511 t512 t513 t514 t515 t516. Prim.Array (React.DOM.DOMProps t516 t515 t514 t513 t512 t511) -> Prim.Array React.UI -> React.UI
foreign import sup :: forall t570 t571 t572 t573 t574 t575. Prim.Array (React.DOM.DOMProps t575 t574 t573 t572 t571 t570) -> Prim.Array React.UI -> React.UI
foreign import summary :: forall t596 t597 t598 t599 t600 t601. Prim.Array (React.DOM.DOMProps t601 t600 t599 t598 t597 t596) -> Prim.Array React.UI -> React.UI
foreign import sub :: forall t622 t623 t624 t625 t626 t627. Prim.Array (React.DOM.DOMProps t627 t626 t625 t624 t623 t622) -> Prim.Array React.UI -> React.UI
foreign import styleDOM :: forall t648 t649 t650 t651 t652 t653. Prim.Array (React.DOM.DOMProps t653 t652 t651 t650 t649 t648) -> Prim.Array React.UI -> React.UI
foreign import strong :: forall t681 t682 t683 t684 t685 t686. Prim.Array (React.DOM.DOMProps t686 t685 t684 t683 t682 t681) -> Prim.Array React.UI -> React.UI
foreign import span :: forall t782 t783 t784 t785 t786 t787. Prim.Array (React.DOM.DOMProps t787 t786 t785 t784 t783 t782) -> Prim.Array React.UI -> React.UI
foreign import source :: forall t808 t809 t810 t811 t812 t813. Prim.Array (React.DOM.DOMProps t813 t812 t811 t810 t809 t808) -> Prim.Array React.UI -> React.UI
foreign import small :: forall t834 t835 t836 t837 t838 t839. Prim.Array (React.DOM.DOMProps t839 t838 t837 t836 t835 t834) -> Prim.Array React.UI -> React.UI
foreign import select :: forall t874 t875 t876 t877 t878 t879. Prim.Array (React.DOM.DOMProps t879 t878 t877 t876 t875 t874) -> Prim.Array React.UI -> React.UI
foreign import section :: forall t900 t901 t902 t903 t904 t905. Prim.Array (React.DOM.DOMProps t905 t904 t903 t902 t901 t900) -> Prim.Array React.UI -> React.UI
foreign import script :: forall t954 t955 t956 t957 t958 t959. Prim.Array (React.DOM.DOMProps t959 t958 t957 t956 t955 t954) -> Prim.Array React.UI -> React.UI
foreign import samp :: forall t994 t995 t996 t997 t998 t999. Prim.Array (React.DOM.DOMProps t999 t998 t997 t996 t995 t994) -> Prim.Array React.UI -> React.UI
foreign import s :: forall t1020 t1021 t1022 t1023 t1024 t1025. Prim.Array (React.DOM.DOMProps t1025 t1024 t1023 t1022 t1021 t1020) -> Prim.Array React.UI -> React.UI
foreign import ruby :: forall t1046 t1047 t1048 t1049 t1050 t1051. Prim.Array (React.DOM.DOMProps t1051 t1050 t1049 t1048 t1047 t1046) -> Prim.Array React.UI -> React.UI
foreign import rt :: forall t1072 t1073 t1074 t1075 t1076 t1077. Prim.Array (React.DOM.DOMProps t1077 t1076 t1075 t1074 t1073 t1072) -> Prim.Array React.UI -> React.UI
foreign import rp :: forall t1098 t1099 t1100 t1101 t1102 t1103. Prim.Array (React.DOM.DOMProps t1103 t1102 t1101 t1100 t1099 t1098) -> Prim.Array React.UI -> React.UI
foreign import q :: forall t1232 t1233 t1234 t1235 t1236 t1237. Prim.Array (React.DOM.DOMProps t1237 t1236 t1235 t1234 t1233 t1232) -> Prim.Array React.UI -> React.UI
foreign import progress :: forall t1265 t1266 t1267 t1268 t1269 t1270. Prim.Array (React.DOM.DOMProps t1270 t1269 t1268 t1267 t1266 t1265) -> Prim.Array React.UI -> React.UI
foreign import pre :: forall t1298 t1299 t1300 t1301 t1302 t1303. Prim.Array (React.DOM.DOMProps t1303 t1302 t1301 t1300 t1299 t1298) -> Prim.Array React.UI -> React.UI
foreign import param :: forall t1423 t1424 t1425 t1426 t1427 t1428. Prim.Array (React.DOM.DOMProps t1428 t1427 t1426 t1425 t1424 t1423) -> Prim.Array React.UI -> React.UI
foreign import p :: forall t1449 t1450 t1451 t1452 t1453 t1454. Prim.Array (React.DOM.DOMProps t1454 t1453 t1452 t1451 t1450 t1449) -> Prim.Array React.UI -> React.UI
foreign import output :: forall t1475 t1476 t1477 t1478 t1479 t1480. Prim.Array (React.DOM.DOMProps t1480 t1479 t1478 t1477 t1476 t1475) -> Prim.Array React.UI -> React.UI
foreign import option :: forall t1501 t1502 t1503 t1504 t1505 t1506. Prim.Array (React.DOM.DOMProps t1506 t1505 t1504 t1503 t1502 t1501) -> Prim.Array React.UI -> React.UI
foreign import optgroup :: forall t1527 t1528 t1529 t1530 t1531 t1532. Prim.Array (React.DOM.DOMProps t1532 t1531 t1530 t1529 t1528 t1527) -> Prim.Array React.UI -> React.UI
foreign import ol :: forall t2693 t2694 t2695 t2696 t2697 t2698. Prim.Array (React.DOM.DOMProps t2698 t2697 t2696 t2695 t2694 t2693) -> Prim.Array React.UI -> React.UI
foreign import object :: forall t2719 t2720 t2721 t2722 t2723 t2724. Prim.Array (React.DOM.DOMProps t2724 t2723 t2722 t2721 t2720 t2719) -> Prim.Array React.UI -> React.UI
foreign import noscript :: forall t2745 t2746 t2747 t2748 t2749 t2750. Prim.Array (React.DOM.DOMProps t2750 t2749 t2748 t2747 t2746 t2745) -> Prim.Array React.UI -> React.UI
foreign import nav :: forall t2778 t2779 t2780 t2781 t2782 t2783. Prim.Array (React.DOM.DOMProps t2783 t2782 t2781 t2780 t2779 t2778) -> Prim.Array React.UI -> React.UI
foreign import meter :: forall t2839 t2840 t2841 t2842 t2843 t2844. Prim.Array (React.DOM.DOMProps t2844 t2843 t2842 t2841 t2840 t2839) -> Prim.Array React.UI -> React.UI
foreign import meta :: forall t2865 t2866 t2867 t2868 t2869 t2870. Prim.Array (React.DOM.DOMProps t2870 t2869 t2868 t2867 t2866 t2865) -> Prim.Array React.UI -> React.UI
foreign import menuitem :: forall t2891 t2892 t2893 t2894 t2895 t2896. Prim.Array (React.DOM.DOMProps t2896 t2895 t2894 t2893 t2892 t2891) -> Prim.Array React.UI -> React.UI
foreign import menu :: forall t2917 t2918 t2919 t2920 t2921 t2922. Prim.Array (React.DOM.DOMProps t2922 t2921 t2920 t2919 t2918 t2917) -> Prim.Array React.UI -> React.UI
foreign import mark :: forall t2964 t2965 t2966 t2967 t2968 t2969. Prim.Array (React.DOM.DOMProps t2969 t2968 t2967 t2966 t2965 t2964) -> Prim.Array React.UI -> React.UI
foreign import mapDOM :: forall t2990 t2991 t2992 t2993 t2994 t2995. Prim.Array (React.DOM.DOMProps t2995 t2994 t2993 t2992 t2991 t2990) -> Prim.Array React.UI -> React.UI
foreign import mainDOM :: forall t3016 t3017 t3018 t3019 t3020 t3021. Prim.Array (React.DOM.DOMProps t3021 t3020 t3019 t3018 t3017 t3016) -> Prim.Array React.UI -> React.UI
foreign import link :: forall t3056 t3057 t3058 t3059 t3060 t3061. Prim.Array (React.DOM.DOMProps t3061 t3060 t3059 t3058 t3057 t3056) -> Prim.Array React.UI -> React.UI
foreign import li :: forall t3134 t3135 t3136 t3137 t3138 t3139. Prim.Array (React.DOM.DOMProps t3139 t3138 t3137 t3136 t3135 t3134) -> Prim.Array React.UI -> React.UI
foreign import legend :: forall t3160 t3161 t3162 t3163 t3164 t3165. Prim.Array (React.DOM.DOMProps t3165 t3164 t3163 t3162 t3161 t3160) -> Prim.Array React.UI -> React.UI
foreign import label :: forall t3200 t3201 t3202 t3203 t3204 t3205. Prim.Array (React.DOM.DOMProps t3205 t3204 t3203 t3202 t3201 t3200) -> Prim.Array React.UI -> React.UI
foreign import keygen :: forall t3226 t3227 t3228 t3229 t3230 t3231. Prim.Array (React.DOM.DOMProps t3231 t3230 t3229 t3228 t3227 t3226) -> Prim.Array React.UI -> React.UI
foreign import kbd :: forall t3259 t3260 t3261 t3262 t3263 t3264. Prim.Array (React.DOM.DOMProps t3264 t3263 t3262 t3261 t3260 t3259) -> Prim.Array React.UI -> React.UI
foreign import ins :: forall t3285 t3286 t3287 t3288 t3289 t3290. Prim.Array (React.DOM.DOMProps t3290 t3289 t3288 t3287 t3286 t3285) -> Prim.Array React.UI -> React.UI
foreign import input :: forall t3311 t3312 t3313 t3314 t3315 t3316. Prim.Array (React.DOM.DOMProps t3316 t3315 t3314 t3313 t3312 t3311) -> Prim.Array React.UI -> React.UI
foreign import img :: forall t3337 t3338 t3339 t3340 t3341 t3342. Prim.Array (React.DOM.DOMProps t3342 t3341 t3340 t3339 t3338 t3337) -> Prim.Array React.UI -> React.UI
foreign import iframe :: forall t3363 t3364 t3365 t3366 t3367 t3368. Prim.Array (React.DOM.DOMProps t3368 t3367 t3366 t3365 t3364 t3363) -> Prim.Array React.UI -> React.UI
foreign import i :: forall t3403 t3404 t3405 t3406 t3407 t3408. Prim.Array (React.DOM.DOMProps t3408 t3407 t3406 t3405 t3404 t3403) -> Prim.Array React.UI -> React.UI
foreign import html :: forall t3443 t3444 t3445 t3446 t3447 t3448. Prim.Array (React.DOM.DOMProps t3448 t3447 t3446 t3445 t3444 t3443) -> Prim.Array React.UI -> React.UI
foreign import hr :: forall t3483 t3484 t3485 t3486 t3487 t3488. Prim.Array (React.DOM.DOMProps t3488 t3487 t3486 t3485 t3484 t3483) -> Prim.Array React.UI -> React.UI
foreign import header :: forall t3523 t3524 t3525 t3526 t3527 t3528. Prim.Array (React.DOM.DOMProps t3528 t3527 t3526 t3525 t3524 t3523) -> Prim.Array React.UI -> React.UI
foreign import headDOM :: forall t3549 t3550 t3551 t3552 t3553 t3554. Prim.Array (React.DOM.DOMProps t3554 t3553 t3552 t3551 t3550 t3549) -> Prim.Array React.UI -> React.UI
foreign import h6 :: forall t3575 t3576 t3577 t3578 t3579 t3580. Prim.Array (React.DOM.DOMProps t3580 t3579 t3578 t3577 t3576 t3575) -> Prim.Array React.UI -> React.UI
foreign import h5 :: forall t3601 t3602 t3603 t3604 t3605 t3606. Prim.Array (React.DOM.DOMProps t3606 t3605 t3604 t3603 t3602 t3601) -> Prim.Array React.UI -> React.UI
foreign import h4 :: forall t3627 t3628 t3629 t3630 t3631 t3632. Prim.Array (React.DOM.DOMProps t3632 t3631 t3630 t3629 t3628 t3627) -> Prim.Array React.UI -> React.UI
foreign import h3 :: forall t3653 t3654 t3655 t3656 t3657 t3658. Prim.Array (React.DOM.DOMProps t3658 t3657 t3656 t3655 t3654 t3653) -> Prim.Array React.UI -> React.UI
foreign import h2 :: forall t3679 t3680 t3681 t3682 t3683 t3684. Prim.Array (React.DOM.DOMProps t3684 t3683 t3682 t3681 t3680 t3679) -> Prim.Array React.UI -> React.UI
foreign import h1 :: forall t3705 t3706 t3707 t3708 t3709 t3710. Prim.Array (React.DOM.DOMProps t3710 t3709 t3708 t3707 t3706 t3705) -> Prim.Array React.UI -> React.UI
foreign import form :: forall t3778 t3779 t3780 t3781 t3782 t3783. Prim.Array (React.DOM.DOMProps t3783 t3782 t3781 t3780 t3779 t3778) -> Prim.Array React.UI -> React.UI
foreign import footer :: forall t3804 t3805 t3806 t3807 t3808 t3809. Prim.Array (React.DOM.DOMProps t3809 t3808 t3807 t3806 t3805 t3804) -> Prim.Array React.UI -> React.UI
foreign import figure :: forall t3830 t3831 t3832 t3833 t3834 t3835. Prim.Array (React.DOM.DOMProps t3835 t3834 t3833 t3832 t3831 t3830) -> Prim.Array React.UI -> React.UI
foreign import figcaption :: forall t3856 t3857 t3858 t3859 t3860 t3861. Prim.Array (React.DOM.DOMProps t3861 t3860 t3859 t3858 t3857 t3856) -> Prim.Array React.UI -> React.UI
foreign import fieldset :: forall t3882 t3883 t3884 t3885 t3886 t3887. Prim.Array (React.DOM.DOMProps t3887 t3886 t3885 t3884 t3883 t3882) -> Prim.Array React.UI -> React.UI
foreign import embed :: forall t3915 t3916 t3917 t3918 t3919 t3920. Prim.Array (React.DOM.DOMProps t3920 t3919 t3918 t3917 t3916 t3915) -> Prim.Array React.UI -> React.UI
foreign import em :: forall t3941 t3942 t3943 t3944 t3945 t3946. Prim.Array (React.DOM.DOMProps t3946 t3945 t3944 t3943 t3942 t3941) -> Prim.Array React.UI -> React.UI
foreign import dt :: forall t3967 t3968 t3969 t3970 t3971 t3972. Prim.Array (React.DOM.DOMProps t3972 t3971 t3970 t3969 t3968 t3967) -> Prim.Array React.UI -> React.UI
foreign import dl :: forall t4007 t4008 t4009 t4010 t4011 t4012. Prim.Array (React.DOM.DOMProps t4012 t4011 t4010 t4009 t4008 t4007) -> Prim.Array React.UI -> React.UI
foreign import div :: forall t4033 t4034 t4035 t4036 t4037 t4038. Prim.Array (React.DOM.DOMProps t4038 t4037 t4036 t4035 t4034 t4033) -> Prim.Array React.UI -> React.UI
foreign import dfn :: forall t4073 t4074 t4075 t4076 t4077 t4078. Prim.Array (React.DOM.DOMProps t4078 t4077 t4076 t4075 t4074 t4073) -> Prim.Array React.UI -> React.UI
foreign import details :: forall t4099 t4100 t4101 t4102 t4103 t4104. Prim.Array (React.DOM.DOMProps t4104 t4103 t4102 t4101 t4100 t4099) -> Prim.Array React.UI -> React.UI
foreign import del :: forall t4125 t4126 t4127 t4128 t4129 t4130. Prim.Array (React.DOM.DOMProps t4130 t4129 t4128 t4127 t4126 t4125) -> Prim.Array React.UI -> React.UI
foreign import dd :: forall t4184 t4185 t4186 t4187 t4188 t4189. Prim.Array (React.DOM.DOMProps t4189 t4188 t4187 t4186 t4185 t4184) -> Prim.Array React.UI -> React.UI
foreign import colgroup :: forall t4274 t4275 t4276 t4277 t4278 t4279. Prim.Array (React.DOM.DOMProps t4279 t4278 t4277 t4276 t4275 t4274) -> Prim.Array React.UI -> React.UI
foreign import col :: forall t4307 t4308 t4309 t4310 t4311 t4312. Prim.Array (React.DOM.DOMProps t4312 t4311 t4310 t4309 t4308 t4307) -> Prim.Array React.UI -> React.UI
foreign import code :: forall t4333 t4334 t4335 t4336 t4337 t4338. Prim.Array (React.DOM.DOMProps t4338 t4337 t4336 t4335 t4334 t4333) -> Prim.Array React.UI -> React.UI
foreign import cite :: forall t4366 t4367 t4368 t4369 t4370 t4371. Prim.Array (React.DOM.DOMProps t4371 t4370 t4369 t4368 t4367 t4366) -> Prim.Array React.UI -> React.UI
foreign import caption :: forall t4446 t4447 t4448 t4449 t4450 t4451. Prim.Array (React.DOM.DOMProps t4451 t4450 t4449 t4448 t4447 t4446) -> Prim.Array React.UI -> React.UI
foreign import canvas :: forall t4472 t4473 t4474 t4475 t4476 t4477. Prim.Array (React.DOM.DOMProps t4477 t4476 t4475 t4474 t4473 t4472) -> Prim.Array React.UI -> React.UI
foreign import button :: forall t4498 t4499 t4500 t4501 t4502 t4503. Prim.Array (React.DOM.DOMProps t4503 t4502 t4501 t4500 t4499 t4498) -> Prim.Array React.UI -> React.UI
foreign import br :: forall t4524 t4525 t4526 t4527 t4528 t4529. Prim.Array (React.DOM.DOMProps t4529 t4528 t4527 t4526 t4525 t4524) -> Prim.Array React.UI -> React.UI
foreign import body :: forall t4550 t4551 t4552 t4553 t4554 t4555. Prim.Array (React.DOM.DOMProps t4555 t4554 t4553 t4552 t4551 t4550) -> Prim.Array React.UI -> React.UI
foreign import blockquote :: forall t4576 t4577 t4578 t4579 t4580 t4581. Prim.Array (React.DOM.DOMProps t4581 t4580 t4579 t4578 t4577 t4576) -> Prim.Array React.UI -> React.UI
foreign import big :: forall t4602 t4603 t4604 t4605 t4606 t4607. Prim.Array (React.DOM.DOMProps t4607 t4606 t4605 t4604 t4603 t4602) -> Prim.Array React.UI -> React.UI
foreign import bdo :: forall t4628 t4629 t4630 t4631 t4632 t4633. Prim.Array (React.DOM.DOMProps t4633 t4632 t4631 t4630 t4629 t4628) -> Prim.Array React.UI -> React.UI
foreign import bdi :: forall t4654 t4655 t4656 t4657 t4658 t4659. Prim.Array (React.DOM.DOMProps t4659 t4658 t4657 t4656 t4655 t4654) -> Prim.Array React.UI -> React.UI
foreign import base :: forall t4680 t4681 t4682 t4683 t4684 t4685. Prim.Array (React.DOM.DOMProps t4685 t4684 t4683 t4682 t4681 t4680) -> Prim.Array React.UI -> React.UI
foreign import b :: forall t4706 t4707 t4708 t4709 t4710 t4711. Prim.Array (React.DOM.DOMProps t4711 t4710 t4709 t4708 t4707 t4706) -> Prim.Array React.UI -> React.UI
foreign import audio :: forall t4767 t4768 t4769 t4770 t4771 t4772. Prim.Array (React.DOM.DOMProps t4772 t4771 t4770 t4769 t4768 t4767) -> Prim.Array React.UI -> React.UI
foreign import aside :: forall t4800 t4801 t4802 t4803 t4804 t4805. Prim.Array (React.DOM.DOMProps t4805 t4804 t4803 t4802 t4801 t4800) -> Prim.Array React.UI -> React.UI
foreign import article :: forall t4826 t4827 t4828 t4829 t4830 t4831. Prim.Array (React.DOM.DOMProps t4831 t4830 t4829 t4828 t4827 t4826) -> Prim.Array React.UI -> React.UI
foreign import area :: forall t4859 t4860 t4861 t4862 t4863 t4864. Prim.Array (React.DOM.DOMProps t4864 t4863 t4862 t4861 t4860 t4859) -> Prim.Array React.UI -> React.UI
foreign import address :: forall t4913 t4914 t4915 t4916 t4917 t4918. Prim.Array (React.DOM.DOMProps t4918 t4917 t4916 t4915 t4914 t4913) -> Prim.Array React.UI -> React.UI
foreign import abbr :: forall t4960 t4961 t4962 t4963 t4964 t4965. Prim.Array (React.DOM.DOMProps t4965 t4964 t4963 t4962 t4961 t4960) -> Prim.Array React.UI -> React.UI
foreign import a :: forall t4986 t4987 t4988 t4989 t4990 t4991. Prim.Array (React.DOM.DOMProps t4991 t4990 t4989 t4988 t4987 t4986) -> Prim.Array React.UI -> React.UI
foreign import onWheel :: forall t1558 t1559 t1560 t1561 t1562 t1563 t1570 t1571 t1572 t1573 t1575. (React.Event -> React.EventHandlerContext t1575 t1573 t1572 t1571 t1570) -> React.DOM.DOMProps t1563 t1562 t1561 t1560 t1559 t1558
foreign import onTouchStart :: forall t1588 t1589 t1590 t1591 t1592 t1593 t1600 t1601 t1602 t1603 t1605. (React.Event -> React.EventHandlerContext t1605 t1603 t1602 t1601 t1600) -> React.DOM.DOMProps t1593 t1592 t1591 t1590 t1589 t1588
foreign import onTouchMove :: forall t1618 t1619 t1620 t1621 t1622 t1623 t1630 t1631 t1632 t1633 t1635. (React.Event -> React.EventHandlerContext t1635 t1633 t1632 t1631 t1630) -> React.DOM.DOMProps t1623 t1622 t1621 t1620 t1619 t1618
foreign import onTouchEnd :: forall t1648 t1649 t1650 t1651 t1652 t1653 t1660 t1661 t1662 t1663 t1665. (React.Event -> React.EventHandlerContext t1665 t1663 t1662 t1661 t1660) -> React.DOM.DOMProps t1653 t1652 t1651 t1650 t1649 t1648
foreign import onTouchCancel :: forall t1678 t1679 t1680 t1681 t1682 t1683 t1690 t1691 t1692 t1693 t1695. (React.Event -> React.EventHandlerContext t1695 t1693 t1692 t1691 t1690) -> React.DOM.DOMProps t1683 t1682 t1681 t1680 t1679 t1678
foreign import onSubmit :: forall t1708 t1709 t1710 t1711 t1712 t1713 t1720 t1721 t1722 t1723 t1725. (React.Event -> React.EventHandlerContext t1725 t1723 t1722 t1721 t1720) -> React.DOM.DOMProps t1713 t1712 t1711 t1710 t1709 t1708
foreign import onScroll :: forall t1738 t1739 t1740 t1741 t1742 t1743 t1750 t1751 t1752 t1753 t1755. (React.Event -> React.EventHandlerContext t1755 t1753 t1752 t1751 t1750) -> React.DOM.DOMProps t1743 t1742 t1741 t1740 t1739 t1738
foreign import onReset :: forall t1768 t1769 t1770 t1771 t1772 t1773 t1780 t1781 t1782 t1783 t1785. (React.Event -> React.EventHandlerContext t1785 t1783 t1782 t1781 t1780) -> React.DOM.DOMProps t1773 t1772 t1771 t1770 t1769 t1768
foreign import onPaste :: forall t1798 t1799 t1800 t1801 t1802 t1803 t1810 t1811 t1812 t1813 t1815. (React.Event -> React.EventHandlerContext t1815 t1813 t1812 t1811 t1810) -> React.DOM.DOMProps t1803 t1802 t1801 t1800 t1799 t1798
foreign import onMouseUp :: forall t1828 t1829 t1830 t1831 t1832 t1833 t1840 t1841 t1842 t1843 t1845. (React.MouseEvent -> React.EventHandlerContext t1845 t1843 t1842 t1841 t1840) -> React.DOM.DOMProps t1833 t1832 t1831 t1830 t1829 t1828
foreign import onMouseOver :: forall t1858 t1859 t1860 t1861 t1862 t1863 t1870 t1871 t1872 t1873 t1875. (React.MouseEvent -> React.EventHandlerContext t1875 t1873 t1872 t1871 t1870) -> React.DOM.DOMProps t1863 t1862 t1861 t1860 t1859 t1858
foreign import onMouseOut :: forall t1888 t1889 t1890 t1891 t1892 t1893 t1900 t1901 t1902 t1903 t1905. (React.MouseEvent -> React.EventHandlerContext t1905 t1903 t1902 t1901 t1900) -> React.DOM.DOMProps t1893 t1892 t1891 t1890 t1889 t1888
foreign import onMouseMove :: forall t1918 t1919 t1920 t1921 t1922 t1923 t1930 t1931 t1932 t1933 t1935. (React.MouseEvent -> React.EventHandlerContext t1935 t1933 t1932 t1931 t1930) -> React.DOM.DOMProps t1923 t1922 t1921 t1920 t1919 t1918
foreign import onMouseDown :: forall t2008 t2009 t2010 t2011 t2012 t2013 t2020 t2021 t2022 t2023 t2025. (React.MouseEvent -> React.EventHandlerContext t2025 t2023 t2022 t2021 t2020) -> React.DOM.DOMProps t2013 t2012 t2011 t2010 t2009 t2008
foreign import onMouseLeave :: forall t1948 t1949 t1950 t1951 t1952 t1953 t1960 t1961 t1962 t1963 t1965. (React.MouseEvent -> React.EventHandlerContext t1965 t1963 t1962 t1961 t1960) -> React.DOM.DOMProps t1953 t1952 t1951 t1950 t1949 t1948
foreign import onMouseEnter :: forall t1978 t1979 t1980 t1981 t1982 t1983 t1990 t1991 t1992 t1993 t1995. (React.MouseEvent -> React.EventHandlerContext t1995 t1993 t1992 t1991 t1990) -> React.DOM.DOMProps t1983 t1982 t1981 t1980 t1979 t1978
foreign import onLoad :: forall t2038 t2039 t2040 t2041 t2042 t2043 t2050 t2051 t2052 t2053 t2055. (React.Event -> React.EventHandlerContext t2055 t2053 t2052 t2051 t2050) -> React.DOM.DOMProps t2043 t2042 t2041 t2040 t2039 t2038
foreign import onKeyUp :: forall t2068 t2069 t2070 t2071 t2072 t2073 t2080 t2081 t2082 t2083 t2085. (React.KeyboardEvent -> React.EventHandlerContext t2085 t2083 t2082 t2081 t2080) -> React.DOM.DOMProps t2073 t2072 t2071 t2070 t2069 t2068
foreign import onKeyPress :: forall t2098 t2099 t2100 t2101 t2102 t2103 t2110 t2111 t2112 t2113 t2115. (React.KeyboardEvent -> React.EventHandlerContext t2115 t2113 t2112 t2111 t2110) -> React.DOM.DOMProps t2103 t2102 t2101 t2100 t2099 t2098
foreign import onKeyDown :: forall t2128 t2129 t2130 t2131 t2132 t2133 t2140 t2141 t2142 t2143 t2145. (React.KeyboardEvent -> React.EventHandlerContext t2145 t2143 t2142 t2141 t2140) -> React.DOM.DOMProps t2133 t2132 t2131 t2130 t2129 t2128
foreign import onInput :: forall t2158 t2159 t2160 t2161 t2162 t2163 t2170 t2171 t2172 t2173 t2175. (React.Event -> React.EventHandlerContext t2175 t2173 t2172 t2171 t2170) -> React.DOM.DOMProps t2163 t2162 t2161 t2160 t2159 t2158
foreign import onFocus :: forall t2188 t2189 t2190 t2191 t2192 t2193 t2200 t2201 t2202 t2203 t2205. (React.Event -> React.EventHandlerContext t2205 t2203 t2202 t2201 t2200) -> React.DOM.DOMProps t2193 t2192 t2191 t2190 t2189 t2188
foreign import onError :: forall t2218 t2219 t2220 t2221 t2222 t2223 t2230 t2231 t2232 t2233 t2235. (React.Event -> React.EventHandlerContext t2235 t2233 t2232 t2231 t2230) -> React.DOM.DOMProps t2223 t2222 t2221 t2220 t2219 t2218
foreign import onDrop :: forall t2248 t2249 t2250 t2251 t2252 t2253 t2260 t2261 t2262 t2263 t2265. (React.Event -> React.EventHandlerContext t2265 t2263 t2262 t2261 t2260) -> React.DOM.DOMProps t2253 t2252 t2251 t2250 t2249 t2248
foreign import onDragStart :: forall t2278 t2279 t2280 t2281 t2282 t2283 t2290 t2291 t2292 t2293 t2295. (React.MouseEvent -> React.EventHandlerContext t2295 t2293 t2292 t2291 t2290) -> React.DOM.DOMProps t2283 t2282 t2281 t2280 t2279 t2278
foreign import onDragOver :: forall t2308 t2309 t2310 t2311 t2312 t2313 t2320 t2321 t2322 t2323 t2325. (React.MouseEvent -> React.EventHandlerContext t2325 t2323 t2322 t2321 t2320) -> React.DOM.DOMProps t2313 t2312 t2311 t2310 t2309 t2308
foreign import onDragLeave :: forall t2338 t2339 t2340 t2341 t2342 t2343 t2350 t2351 t2352 t2353 t2355. (React.MouseEvent -> React.EventHandlerContext t2355 t2353 t2352 t2351 t2350) -> React.DOM.DOMProps t2343 t2342 t2341 t2340 t2339 t2338
foreign import onDragExit :: forall t2368 t2369 t2370 t2371 t2372 t2373 t2380 t2381 t2382 t2383 t2385. (React.MouseEvent -> React.EventHandlerContext t2385 t2383 t2382 t2381 t2380) -> React.DOM.DOMProps t2373 t2372 t2371 t2370 t2369 t2368
foreign import onDragEnter :: forall t2398 t2399 t2400 t2401 t2402 t2403 t2410 t2411 t2412 t2413 t2415. (React.MouseEvent -> React.EventHandlerContext t2415 t2413 t2412 t2411 t2410) -> React.DOM.DOMProps t2403 t2402 t2401 t2400 t2399 t2398
foreign import onDragEnd :: forall t2428 t2429 t2430 t2431 t2432 t2433 t2440 t2441 t2442 t2443 t2445. (React.MouseEvent -> React.EventHandlerContext t2445 t2443 t2442 t2441 t2440) -> React.DOM.DOMProps t2433 t2432 t2431 t2430 t2429 t2428
foreign import onDrag :: forall t2458 t2459 t2460 t2461 t2462 t2463 t2470 t2471 t2472 t2473 t2475. (React.MouseEvent -> React.EventHandlerContext t2475 t2473 t2472 t2471 t2470) -> React.DOM.DOMProps t2463 t2462 t2461 t2460 t2459 t2458
foreign import onDoubleClick :: forall t2488 t2489 t2490 t2491 t2492 t2493 t2500 t2501 t2502 t2503 t2505. (React.MouseEvent -> React.EventHandlerContext t2505 t2503 t2502 t2501 t2500) -> React.DOM.DOMProps t2493 t2492 t2491 t2490 t2489 t2488
foreign import onCut :: forall t2518 t2519 t2520 t2521 t2522 t2523 t2530 t2531 t2532 t2533 t2535. (React.Event -> React.EventHandlerContext t2535 t2533 t2532 t2531 t2530) -> React.DOM.DOMProps t2523 t2522 t2521 t2520 t2519 t2518
foreign import onCopy :: forall t2548 t2549 t2550 t2551 t2552 t2553 t2560 t2561 t2562 t2563 t2565. (React.Event -> React.EventHandlerContext t2565 t2563 t2562 t2561 t2560) -> React.DOM.DOMProps t2553 t2552 t2551 t2550 t2549 t2548
foreign import onContextMenu :: forall t2578 t2579 t2580 t2581 t2582 t2583 t2590 t2591 t2592 t2593 t2595. (React.Event -> React.EventHandlerContext t2595 t2593 t2592 t2591 t2590) -> React.DOM.DOMProps t2583 t2582 t2581 t2580 t2579 t2578
foreign import onClick :: forall t2608 t2609 t2610 t2611 t2612 t2613 t2620 t2621 t2622 t2623 t2625. (React.MouseEvent -> React.EventHandlerContext t2625 t2623 t2622 t2621 t2620) -> React.DOM.DOMProps t2613 t2612 t2611 t2610 t2609 t2608
foreign import onChange :: forall t2638 t2639 t2640 t2641 t2642 t2643 t2650 t2651 t2652 t2653 t2655. (React.Event -> React.EventHandlerContext t2655 t2653 t2652 t2651 t2650) -> React.DOM.DOMProps t2643 t2642 t2641 t2640 t2639 t2638
foreign import onBlur :: forall t2668 t2669 t2670 t2671 t2672 t2673 t2680 t2681 t2682 t2683 t2685. (React.Event -> React.EventHandlerContext t2685 t2683 t2682 t2681 t2680) -> React.DOM.DOMProps t2673 t2672 t2671 t2670 t2669 t2668
foreign import value :: forall t165 t166 t167 t168 t169 t170. Prim.String -> React.DOM.DOMProps t170 t169 t168 t167 t166 t165
foreign import dangerouslySetInnerHTML :: forall t4219 t4220 t4221 t4222 t4223 t4224. Prim.String -> React.DOM.DOMProps t4224 t4223 t4222 t4221 t4220 t4219
foreign import key :: forall t3246 t3247 t3248 t3249 t3250 t3251. Prim.String -> React.DOM.DOMProps t3251 t3250 t3249 t3248 t3247 t3246
foreign import ref :: forall t1153 t1154 t1155 t1156 t1157 t1158. Prim.String -> React.DOM.DOMProps t1158 t1157 t1156 t1155 t1154 t1153
foreign import property :: forall t1252 t1253 t1254 t1255 t1256 t1257. Prim.String -> React.DOM.DOMProps t1257 t1256 t1255 t1254 t1253 t1252
foreign import autoCorrect :: forall t4740 t4741 t4742 t4743 t4744 t4745. Prim.String -> React.DOM.DOMProps t4745 t4744 t4743 t4742 t4741 t4740
foreign import autoCapitalize :: forall t4754 t4755 t4756 t4757 t4758 t4759. Prim.String -> React.DOM.DOMProps t4759 t4758 t4757 t4756 t4755 t4754
foreign import wmode :: forall t73 t74 t75 t76 t77 t78. Prim.String -> React.DOM.DOMProps t78 t77 t76 t75 t74 t73
foreign import width :: forall t80 t81 t82 t83 t84 t85. Prim.String -> React.DOM.DOMProps t85 t84 t83 t82 t81 t80
foreign import alue :: forall t4879 t4880 t4881 t4882 t4883 t4884. Prim.String -> React.DOM.DOMProps t4884 t4883 t4882 t4881 t4880 t4879
foreign import typeProp :: forall t224 t225 t226 t227 t228 t229. Prim.String -> React.DOM.DOMProps t229 t228 t227 t226 t225 t224
foreign import titleProp :: forall t283 t284 t285 t286 t287 t288. Prim.String -> React.DOM.DOMProps t288 t287 t286 t285 t284 t283
foreign import target :: forall t498 t499 t500 t501 t502 t503. Prim.String -> React.DOM.DOMProps t503 t502 t501 t500 t499 t498
foreign import tabIndex :: forall t531 t532 t533 t534 t535 t536. Prim.String -> React.DOM.DOMProps t536 t535 t534 t533 t532 t531
foreign import style :: forall t668 t669 t670 t671 t672 t673. {  | t673 } -> React.DOM.DOMProps t673 t672 t671 t670 t669 t668
foreign import step :: forall t727 t728 t729 t730 t731 t732. Prim.String -> React.DOM.DOMProps t732 t731 t730 t729 t728 t727
foreign import start :: forall t734 t735 t736 t737 t738 t739. Prim.String -> React.DOM.DOMProps t739 t738 t737 t736 t735 t734
foreign import srcSet :: forall t741 t742 t743 t744 t745 t746. Prim.String -> React.DOM.DOMProps t746 t745 t744 t743 t742 t741
foreign import srcDoc :: forall t748 t749 t750 t751 t752 t753. Prim.String -> React.DOM.DOMProps t753 t752 t751 t750 t749 t748
foreign import src :: forall t755 t756 t757 t758 t759 t760. Prim.String -> React.DOM.DOMProps t760 t759 t758 t757 t756 t755
foreign import spellCheck :: forall t762 t763 t764 t765 t766 t767. Prim.String -> React.DOM.DOMProps t767 t766 t765 t764 t763 t762
foreign import spanProp :: forall t769 t770 t771 t772 t773 t774. Prim.String -> React.DOM.DOMProps t774 t773 t772 t771 t770 t769
foreign import size :: forall t854 t855 t856 t857 t858 t859. Prim.String -> React.DOM.DOMProps t859 t858 t857 t856 t855 t854
foreign import selected :: forall t861 t862 t863 t864 t865 t866. Prim.String -> React.DOM.DOMProps t866 t865 t864 t863 t862 t861
foreign import seamless :: forall t920 t921 t922 t923 t924 t925. Prim.String -> React.DOM.DOMProps t925 t924 t923 t922 t921 t920
foreign import scrollTop :: forall t934 t935 t936 t937 t938 t939. Prim.String -> React.DOM.DOMProps t939 t938 t937 t936 t935 t934
foreign import scrolling :: forall t927 t928 t929 t930 t931 t932. Prim.String -> React.DOM.DOMProps t932 t931 t930 t929 t928 t927
foreign import scrollLeft :: forall t941 t942 t943 t944 t945 t946. Prim.String -> React.DOM.DOMProps t946 t945 t944 t943 t942 t941
foreign import scope :: forall t974 t975 t976 t977 t978 t979. Prim.String -> React.DOM.DOMProps t979 t978 t977 t976 t975 t974
foreign import sandbox :: forall t981 t982 t983 t984 t985 t986. Prim.String -> React.DOM.DOMProps t986 t985 t984 t983 t982 t981
foreign import rowSpan :: forall t1125 t1126 t1127 t1128 t1129 t1130. Prim.String -> React.DOM.DOMProps t1130 t1129 t1128 t1127 t1126 t1125
foreign import rows :: forall t1118 t1119 t1120 t1121 t1122 t1123. Prim.String -> React.DOM.DOMProps t1123 t1122 t1121 t1120 t1119 t1118
foreign import role :: forall t1132 t1133 t1134 t1135 t1136 t1137. Prim.String -> React.DOM.DOMProps t1137 t1136 t1135 t1134 t1133 t1132
foreign import required :: forall t1139 t1140 t1141 t1142 t1143 t1144. Prim.String -> React.DOM.DOMProps t1144 t1143 t1142 t1141 t1140 t1139
foreign import rel :: forall t1146 t1147 t1148 t1149 t1150 t1151. Prim.String -> React.DOM.DOMProps t1151 t1150 t1149 t1148 t1147 t1146
foreign import readOnly :: forall t1186 t1187 t1188 t1189 t1190 t1191. Prim.String -> React.DOM.DOMProps t1191 t1190 t1189 t1188 t1187 t1186
foreign import radioGroup :: forall t1193 t1194 t1195 t1196 t1197 t1198. Prim.String -> React.DOM.DOMProps t1198 t1197 t1196 t1195 t1194 t1193
foreign import preload :: forall t1285 t1286 t1287 t1288 t1289 t1290. Prim.String -> React.DOM.DOMProps t1290 t1289 t1288 t1287 t1286 t1285
foreign import poster :: forall t1318 t1319 t1320 t1321 t1322 t1323. Prim.String -> React.DOM.DOMProps t1323 t1322 t1321 t1320 t1319 t1318
foreign import placeholder :: forall t1377 t1378 t1379 t1380 t1381 t1382. Prim.String -> React.DOM.DOMProps t1382 t1381 t1380 t1379 t1378 t1377
foreign import pattern :: forall t1384 t1385 t1386 t1387 t1388 t1389. Prim.String -> React.DOM.DOMProps t1389 t1388 t1387 t1386 t1385 t1384
foreign import noValidate :: forall t2765 t2766 t2767 t2768 t2769 t2770. Prim.String -> React.DOM.DOMProps t2770 t2769 t2768 t2767 t2766 t2765
foreign import name :: forall t2798 t2799 t2800 t2801 t2802 t2803. Prim.String -> React.DOM.DOMProps t2803 t2802 t2801 t2800 t2799 t2798
foreign import muted :: forall t2805 t2806 t2807 t2808 t2809 t2810. Prim.String -> React.DOM.DOMProps t2810 t2809 t2808 t2807 t2806 t2805
foreign import multiple :: forall t2812 t2813 t2814 t2815 t2816 t2817. Prim.String -> React.DOM.DOMProps t2817 t2816 t2815 t2814 t2813 t2812
foreign import min :: forall t2819 t2820 t2821 t2822 t2823 t2824. Prim.String -> React.DOM.DOMProps t2824 t2823 t2822 t2821 t2820 t2819
foreign import method :: forall t2826 t2827 t2828 t2829 t2830 t2831. Prim.String -> React.DOM.DOMProps t2831 t2830 t2829 t2828 t2827 t2826
foreign import mediaGroup :: forall t2937 t2938 t2939 t2940 t2941 t2942. Prim.String -> React.DOM.DOMProps t2942 t2941 t2940 t2939 t2938 t2937
foreign import maxLength :: forall t2944 t2945 t2946 t2947 t2948 t2949. Prim.String -> React.DOM.DOMProps t2949 t2948 t2947 t2946 t2945 t2944
foreign import max :: forall t2951 t2952 t2953 t2954 t2955 t2956. Prim.String -> React.DOM.DOMProps t2956 t2955 t2954 t2953 t2952 t2951
foreign import loop :: forall t3036 t3037 t3038 t3039 t3040 t3041. Prim.String -> React.DOM.DOMProps t3041 t3040 t3039 t3038 t3037 t3036
foreign import list :: forall t3043 t3044 t3045 t3046 t3047 t3048. Prim.String -> React.DOM.DOMProps t3048 t3047 t3046 t3045 t3044 t3043
foreign import lang :: forall t3180 t3181 t3182 t3183 t3184 t3185. Prim.String -> React.DOM.DOMProps t3185 t3184 t3183 t3182 t3181 t3180
foreign import labelProp :: forall t3187 t3188 t3189 t3190 t3191 t3192. Prim.String -> React.DOM.DOMProps t3192 t3191 t3190 t3189 t3188 t3187
foreign import idProp :: forall t3383 t3384 t3385 t3386 t3387 t3388. Prim.String -> React.DOM.DOMProps t3388 t3387 t3386 t3385 t3384 t3383
foreign import icon :: forall t3390 t3391 t3392 t3393 t3394 t3395. Prim.String -> React.DOM.DOMProps t3395 t3394 t3393 t3392 t3391 t3390
foreign import httpEquiv :: forall t3423 t3424 t3425 t3426 t3427 t3428. Prim.String -> React.DOM.DOMProps t3428 t3427 t3426 t3425 t3424 t3423
foreign import htmlFor :: forall t3430 t3431 t3432 t3433 t3434 t3435. Prim.String -> React.DOM.DOMProps t3435 t3434 t3433 t3432 t3431 t3430
foreign import hrefLang :: forall t3463 t3464 t3465 t3466 t3467 t3468. Prim.String -> React.DOM.DOMProps t3468 t3467 t3466 t3465 t3464 t3463
foreign import href :: forall t3470 t3471 t3472 t3473 t3474 t3475. Prim.String -> React.DOM.DOMProps t3475 t3474 t3473 t3472 t3471 t3470
foreign import hidden :: forall t3503 t3504 t3505 t3506 t3507 t3508. Prim.String -> React.DOM.DOMProps t3508 t3507 t3506 t3505 t3504 t3503
foreign import height :: forall t3510 t3511 t3512 t3513 t3514 t3515. Prim.String -> React.DOM.DOMProps t3515 t3514 t3513 t3512 t3511 t3510
foreign import frameBorder :: forall t3751 t3752 t3753 t3754 t3755 t3756. Prim.String -> React.DOM.DOMProps t3756 t3755 t3754 t3753 t3752 t3751
foreign import formNoValidate :: forall t3765 t3766 t3767 t3768 t3769 t3770. Prim.String -> React.DOM.DOMProps t3770 t3769 t3768 t3767 t3766 t3765
foreign import formProp :: forall t3758 t3759 t3760 t3761 t3762 t3763. Prim.String -> React.DOM.DOMProps t3763 t3762 t3761 t3760 t3759 t3758
foreign import encType :: forall t3902 t3903 t3904 t3905 t3906 t3907. Prim.String -> React.DOM.DOMProps t3907 t3906 t3905 t3904 t3903 t3902
foreign import draggable :: forall t3987 t3988 t3989 t3990 t3991 t3992. Prim.String -> React.DOM.DOMProps t3992 t3991 t3990 t3989 t3988 t3987
foreign import download :: forall t3994 t3995 t3996 t3997 t3998 t3999. Prim.String -> React.DOM.DOMProps t3999 t3998 t3997 t3996 t3995 t3994
foreign import disabled :: forall t4053 t4054 t4055 t4056 t4057 t4058. Prim.String -> React.DOM.DOMProps t4058 t4057 t4056 t4055 t4054 t4053
foreign import dir :: forall t4060 t4061 t4062 t4063 t4064 t4065. Prim.String -> React.DOM.DOMProps t4065 t4064 t4063 t4062 t4061 t4060
foreign import defer :: forall t4171 t4172 t4173 t4174 t4175 t4176. Prim.String -> React.DOM.DOMProps t4176 t4175 t4174 t4173 t4172 t4171
foreign import dateTime :: forall t4204 t4205 t4206 t4207 t4208 t4209. Prim.String -> React.DOM.DOMProps t4209 t4208 t4207 t4206 t4205 t4204
foreign import dataSet :: forall t4211 t4212 t4213 t4214 t4215 t4216. {  | t4215 } -> React.DOM.DOMProps t4216 t4215 t4214 t4213 t4212 t4211
foreign import crossOrigin :: forall t4226 t4227 t4228 t4229 t4230 t4231. Prim.String -> React.DOM.DOMProps t4231 t4230 t4229 t4228 t4227 t4226
foreign import controls :: forall t4233 t4234 t4235 t4236 t4237 t4238. Prim.String -> React.DOM.DOMProps t4238 t4237 t4236 t4235 t4234 t4233
foreign import contextMenu :: forall t4240 t4241 t4242 t4243 t4244 t4245. Prim.String -> React.DOM.DOMProps t4245 t4244 t4243 t4242 t4241 t4240
foreign import contentEditable :: forall t4247 t4248 t4249 t4250 t4251 t4252. Prim.String -> React.DOM.DOMProps t4252 t4251 t4250 t4249 t4248 t4247
foreign import content :: forall t4254 t4255 t4256 t4257 t4258 t4259. Prim.String -> React.DOM.DOMProps t4259 t4258 t4257 t4256 t4255 t4254
foreign import colSpan :: forall t4294 t4295 t4296 t4297 t4298 t4299. Prim.String -> React.DOM.DOMProps t4299 t4298 t4297 t4296 t4295 t4294
foreign import cols :: forall t4261 t4262 t4263 t4264 t4265 t4266. Prim.String -> React.DOM.DOMProps t4266 t4265 t4264 t4263 t4262 t4261
foreign import className :: forall t4353 t4354 t4355 t4356 t4357 t4358. Prim.String -> React.DOM.DOMProps t4358 t4357 t4356 t4355 t4354 t4353
foreign import checked :: forall t4412 t4413 t4414 t4415 t4416 t4417. Prim.String -> React.DOM.DOMProps t4417 t4416 t4415 t4414 t4413 t4412
foreign import charSet :: forall t4419 t4420 t4421 t4422 t4423 t4424. Prim.String -> React.DOM.DOMProps t4424 t4423 t4422 t4421 t4420 t4419
foreign import cellSpacing :: forall t4426 t4427 t4428 t4429 t4430 t4431. Prim.String -> React.DOM.DOMProps t4431 t4430 t4429 t4428 t4427 t4426
foreign import cellPadding :: forall t4433 t4434 t4435 t4436 t4437 t4438. Prim.String -> React.DOM.DOMProps t4438 t4437 t4436 t4435 t4434 t4433
foreign import autoPlay :: forall t4726 t4727 t4728 t4729 t4730 t4731. Prim.String -> React.DOM.DOMProps t4731 t4730 t4729 t4728 t4727 t4726
foreign import autoFocus :: forall t4733 t4734 t4735 t4736 t4737 t4738. Prim.String -> React.DOM.DOMProps t4738 t4737 t4736 t4735 t4734 t4733
foreign import ariaSet :: forall t4846 t4847 t4848 t4849 t4850 t4851. {  | t4849 } -> React.DOM.DOMProps t4851 t4850 t4849 t4848 t4847 t4846
foreign import autoComplete :: forall t4747 t4748 t4749 t4750 t4751 t4752. Prim.String -> React.DOM.DOMProps t4752 t4751 t4750 t4749 t4748 t4747
foreign import async :: forall t4787 t4788 t4789 t4790 t4791 t4792. Prim.String -> React.DOM.DOMProps t4792 t4791 t4790 t4789 t4788 t4787
foreign import alt :: forall t4886 t4887 t4888 t4889 t4890 t4891. Prim.String -> React.DOM.DOMProps t4891 t4890 t4889 t4888 t4887 t4886
foreign import allowTransparency :: forall t4893 t4894 t4895 t4896 t4897 t4898. Prim.String -> React.DOM.DOMProps t4898 t4897 t4896 t4895 t4894 t4893
foreign import allowFullScreen :: forall t4900 t4901 t4902 t4903 t4904 t4905. Prim.String -> React.DOM.DOMProps t4905 t4904 t4903 t4902 t4901 t4900
foreign import action :: forall t4933 t4934 t4935 t4936 t4937 t4938. Prim.String -> React.DOM.DOMProps t4938 t4937 t4936 t4935 t4934 t4933
foreign import accessKey :: forall t4940 t4941 t4942 t4943 t4944 t4945. Prim.String -> React.DOM.DOMProps t4945 t4944 t4943 t4942 t4941 t4940
foreign import accept :: forall t4947 t4948 t4949 t4950 t4951 t4952. Prim.String -> React.DOM.DOMProps t4952 t4951 t4950 t4949 t4948 t4947
foreign import text :: Prim.String -> React.UI
foreign import mkDOM :: forall s dataAttrs ariaAttrs eff props state. Prim.String -> Prim.Array (React.DOM.DOMProps s dataAttrs ariaAttrs eff props state) -> Prim.Array React.UI -> React.UI