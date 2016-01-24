module React.DOM where

import React (ReactElement(), TagName(), createElementTagName, createElementTagNameDynamic)
import React.DOM.Props (Props(), unsafeFromPropsArray)

newtype IsDynamic = IsDynamic Boolean

mkDOM :: IsDynamic -> TagName -> Array Props -> Array ReactElement -> ReactElement
mkDOM dynamic tag props = createElement tag (unsafeFromPropsArray props)
  where
  createElement :: TagName -> Array Props -> Array ReactElement -> ReactElement
  createElement =
    case dynamic of
         IsDynamic false -> createElementTagName
         IsDynamic true -> createElementTagNameDynamic

foreign import text :: String -> ReactElement

a :: Array Props -> Array ReactElement -> ReactElement
a = mkDOM (IsDynamic false) "a"

a' :: Array ReactElement -> ReactElement
a' = a []

abbr :: Array Props -> Array ReactElement -> ReactElement
abbr = mkDOM (IsDynamic false) "abbr"

abbr' :: Array ReactElement -> ReactElement
abbr' = abbr []

address :: Array Props -> Array ReactElement -> ReactElement
address = mkDOM (IsDynamic false) "address"

address' :: Array ReactElement -> ReactElement
address' = address []

area :: Array Props -> Array ReactElement -> ReactElement
area = mkDOM (IsDynamic false) "area"

area' :: Array ReactElement -> ReactElement
area' = area []

article :: Array Props -> Array ReactElement -> ReactElement
article = mkDOM (IsDynamic false) "article"

article' :: Array ReactElement -> ReactElement
article' = article []

aside :: Array Props -> Array ReactElement -> ReactElement
aside = mkDOM (IsDynamic false) "aside"

aside' :: Array ReactElement -> ReactElement
aside' = aside []

audio :: Array Props -> Array ReactElement -> ReactElement
audio = mkDOM (IsDynamic false) "audio"

audio' :: Array ReactElement -> ReactElement
audio' = audio []

b :: Array Props -> Array ReactElement -> ReactElement
b = mkDOM (IsDynamic false) "b"

b' :: Array ReactElement -> ReactElement
b' = b []

base :: Array Props -> Array ReactElement -> ReactElement
base = mkDOM (IsDynamic false) "base"

base' :: Array ReactElement -> ReactElement
base' = base []

bdi :: Array Props -> Array ReactElement -> ReactElement
bdi = mkDOM (IsDynamic false) "bdi"

bdi' :: Array ReactElement -> ReactElement
bdi' = bdi []

bdo :: Array Props -> Array ReactElement -> ReactElement
bdo = mkDOM (IsDynamic false) "bdo"

bdo' :: Array ReactElement -> ReactElement
bdo' = bdo []

big :: Array Props -> Array ReactElement -> ReactElement
big = mkDOM (IsDynamic false) "big"

big' :: Array ReactElement -> ReactElement
big' = big []

blockquote :: Array Props -> Array ReactElement -> ReactElement
blockquote = mkDOM (IsDynamic false) "blockquote"

blockquote' :: Array ReactElement -> ReactElement
blockquote' = blockquote []

body :: Array Props -> Array ReactElement -> ReactElement
body = mkDOM (IsDynamic false) "body"

body' :: Array ReactElement -> ReactElement
body' = body []

br :: Array Props -> Array ReactElement -> ReactElement
br = mkDOM (IsDynamic false) "br"

br' :: Array ReactElement -> ReactElement
br' = br []

button :: Array Props -> Array ReactElement -> ReactElement
button = mkDOM (IsDynamic false) "button"

button' :: Array ReactElement -> ReactElement
button' = button []

canvas :: Array Props -> Array ReactElement -> ReactElement
canvas = mkDOM (IsDynamic false) "canvas"

canvas' :: Array ReactElement -> ReactElement
canvas' = canvas []

caption :: Array Props -> Array ReactElement -> ReactElement
caption = mkDOM (IsDynamic false) "caption"

caption' :: Array ReactElement -> ReactElement
caption' = caption []

cite :: Array Props -> Array ReactElement -> ReactElement
cite = mkDOM (IsDynamic false) "cite"

cite' :: Array ReactElement -> ReactElement
cite' = cite []

code :: Array Props -> Array ReactElement -> ReactElement
code = mkDOM (IsDynamic false) "code"

code' :: Array ReactElement -> ReactElement
code' = code []

col :: Array Props -> Array ReactElement -> ReactElement
col = mkDOM (IsDynamic false) "col"

col' :: Array ReactElement -> ReactElement
col' = col []

colgroup :: Array Props -> Array ReactElement -> ReactElement
colgroup = mkDOM (IsDynamic false) "colgroup"

colgroup' :: Array ReactElement -> ReactElement
colgroup' = colgroup []

_data :: Array Props -> Array ReactElement -> ReactElement
_data = mkDOM (IsDynamic false) "data"

_data' :: Array ReactElement -> ReactElement
_data' = _data []

datalist :: Array Props -> Array ReactElement -> ReactElement
datalist = mkDOM (IsDynamic false) "datalist"

datalist' :: Array ReactElement -> ReactElement
datalist' = datalist []

dd :: Array Props -> Array ReactElement -> ReactElement
dd = mkDOM (IsDynamic false) "dd"

dd' :: Array ReactElement -> ReactElement
dd' = dd []

del :: Array Props -> Array ReactElement -> ReactElement
del = mkDOM (IsDynamic false) "del"

del' :: Array ReactElement -> ReactElement
del' = del []

details :: Array Props -> Array ReactElement -> ReactElement
details = mkDOM (IsDynamic false) "details"

details' :: Array ReactElement -> ReactElement
details' = details []

dfn :: Array Props -> Array ReactElement -> ReactElement
dfn = mkDOM (IsDynamic false) "dfn"

dfn' :: Array ReactElement -> ReactElement
dfn' = dfn []

dialog :: Array Props -> Array ReactElement -> ReactElement
dialog = mkDOM (IsDynamic false) "dialog"

dialog' :: Array ReactElement -> ReactElement
dialog' = dialog []

div :: Array Props -> Array ReactElement -> ReactElement
div = mkDOM (IsDynamic false) "div"

div' :: Array ReactElement -> ReactElement
div' = div []

dl :: Array Props -> Array ReactElement -> ReactElement
dl = mkDOM (IsDynamic false) "dl"

dl' :: Array ReactElement -> ReactElement
dl' = dl []

dt :: Array Props -> Array ReactElement -> ReactElement
dt = mkDOM (IsDynamic false) "dt"

dt' :: Array ReactElement -> ReactElement
dt' = dt []

em :: Array Props -> Array ReactElement -> ReactElement
em = mkDOM (IsDynamic false) "em"

em' :: Array ReactElement -> ReactElement
em' = em []

embed :: Array Props -> Array ReactElement -> ReactElement
embed = mkDOM (IsDynamic false) "embed"

embed' :: Array ReactElement -> ReactElement
embed' = embed []

fieldset :: Array Props -> Array ReactElement -> ReactElement
fieldset = mkDOM (IsDynamic false) "fieldset"

fieldset' :: Array ReactElement -> ReactElement
fieldset' = fieldset []

figcaption :: Array Props -> Array ReactElement -> ReactElement
figcaption = mkDOM (IsDynamic false) "figcaption"

figcaption' :: Array ReactElement -> ReactElement
figcaption' = figcaption []

figure :: Array Props -> Array ReactElement -> ReactElement
figure = mkDOM (IsDynamic false) "figure"

figure' :: Array ReactElement -> ReactElement
figure' = figure []

footer :: Array Props -> Array ReactElement -> ReactElement
footer = mkDOM (IsDynamic false) "footer"

footer' :: Array ReactElement -> ReactElement
footer' = footer []

form :: Array Props -> Array ReactElement -> ReactElement
form = mkDOM (IsDynamic false) "form"

form' :: Array ReactElement -> ReactElement
form' = form []

h1 :: Array Props -> Array ReactElement -> ReactElement
h1 = mkDOM (IsDynamic false) "h1"

h1' :: Array ReactElement -> ReactElement
h1' = h1 []

h2 :: Array Props -> Array ReactElement -> ReactElement
h2 = mkDOM (IsDynamic false) "h2"

h2' :: Array ReactElement -> ReactElement
h2' = h2 []

h3 :: Array Props -> Array ReactElement -> ReactElement
h3 = mkDOM (IsDynamic false) "h3"

h3' :: Array ReactElement -> ReactElement
h3' = h3 []

h4 :: Array Props -> Array ReactElement -> ReactElement
h4 = mkDOM (IsDynamic false) "h4"

h4' :: Array ReactElement -> ReactElement
h4' = h4 []

h5 :: Array Props -> Array ReactElement -> ReactElement
h5 = mkDOM (IsDynamic false) "h5"

h5' :: Array ReactElement -> ReactElement
h5' = h5 []

h6 :: Array Props -> Array ReactElement -> ReactElement
h6 = mkDOM (IsDynamic false) "h6"

h6' :: Array ReactElement -> ReactElement
h6' = h6 []

head :: Array Props -> Array ReactElement -> ReactElement
head = mkDOM (IsDynamic false) "head"

head' :: Array ReactElement -> ReactElement
head' = head []

header :: Array Props -> Array ReactElement -> ReactElement
header = mkDOM (IsDynamic false) "header"

header' :: Array ReactElement -> ReactElement
header' = header []

hr :: Array Props -> Array ReactElement -> ReactElement
hr = mkDOM (IsDynamic false) "hr"

hr' :: Array ReactElement -> ReactElement
hr' = hr []

html :: Array Props -> Array ReactElement -> ReactElement
html = mkDOM (IsDynamic false) "html"

html' :: Array ReactElement -> ReactElement
html' = html []

i :: Array Props -> Array ReactElement -> ReactElement
i = mkDOM (IsDynamic false) "i"

i' :: Array ReactElement -> ReactElement
i' = i []

iframe :: Array Props -> Array ReactElement -> ReactElement
iframe = mkDOM (IsDynamic false) "iframe"

iframe' :: Array ReactElement -> ReactElement
iframe' = iframe []

img :: Array Props -> Array ReactElement -> ReactElement
img = mkDOM (IsDynamic false) "img"

img' :: Array ReactElement -> ReactElement
img' = img []

input :: Array Props -> Array ReactElement -> ReactElement
input = mkDOM (IsDynamic false) "input"

input' :: Array ReactElement -> ReactElement
input' = input []

ins :: Array Props -> Array ReactElement -> ReactElement
ins = mkDOM (IsDynamic false) "ins"

ins' :: Array ReactElement -> ReactElement
ins' = ins []

kbd :: Array Props -> Array ReactElement -> ReactElement
kbd = mkDOM (IsDynamic false) "kbd"

kbd' :: Array ReactElement -> ReactElement
kbd' = kbd []

keygen :: Array Props -> Array ReactElement -> ReactElement
keygen = mkDOM (IsDynamic false) "keygen"

keygen' :: Array ReactElement -> ReactElement
keygen' = keygen []

label :: Array Props -> Array ReactElement -> ReactElement
label = mkDOM (IsDynamic false) "label"

label' :: Array ReactElement -> ReactElement
label' = label []

legend :: Array Props -> Array ReactElement -> ReactElement
legend = mkDOM (IsDynamic false) "legend"

legend' :: Array ReactElement -> ReactElement
legend' = legend []

li :: Array Props -> Array ReactElement -> ReactElement
li = mkDOM (IsDynamic false) "li"

li' :: Array ReactElement -> ReactElement
li' = li []

link :: Array Props -> Array ReactElement -> ReactElement
link = mkDOM (IsDynamic false) "link"

link' :: Array ReactElement -> ReactElement
link' = body []

main :: Array Props -> Array ReactElement -> ReactElement
main = mkDOM (IsDynamic false) "main"

main' :: Array ReactElement -> ReactElement
main' = main []

map :: Array Props -> Array ReactElement -> ReactElement
map = mkDOM (IsDynamic false) "map"

map' :: Array ReactElement -> ReactElement
map' = map []

mark :: Array Props -> Array ReactElement -> ReactElement
mark = mkDOM (IsDynamic false) "mark"

mark' :: Array ReactElement -> ReactElement
mark' = mark []

menu :: Array Props -> Array ReactElement -> ReactElement
menu = mkDOM (IsDynamic false) "menu"

menu' :: Array ReactElement -> ReactElement
menu' = menu []

menuitem :: Array Props -> Array ReactElement -> ReactElement
menuitem = mkDOM (IsDynamic false) "menuitem"

menuitem' :: Array ReactElement -> ReactElement
menuitem' = menuitem []

meta :: Array Props -> Array ReactElement -> ReactElement
meta = mkDOM (IsDynamic false) "meta"

meta' :: Array ReactElement -> ReactElement
meta' = meta []

meter :: Array Props -> Array ReactElement -> ReactElement
meter = mkDOM (IsDynamic false) "meter"

meter' :: Array ReactElement -> ReactElement
meter' = meter []

nav :: Array Props -> Array ReactElement -> ReactElement
nav = mkDOM (IsDynamic false) "nav"

nav' :: Array ReactElement -> ReactElement
nav' = nav []

noscript :: Array Props -> Array ReactElement -> ReactElement
noscript = mkDOM (IsDynamic false) "noscript"

noscript' :: Array ReactElement -> ReactElement
noscript' = noscript []

object :: Array Props -> Array ReactElement -> ReactElement
object = mkDOM (IsDynamic false) "object"

object' :: Array ReactElement -> ReactElement
object' = object []

ol :: Array Props -> Array ReactElement -> ReactElement
ol = mkDOM (IsDynamic false) "ol"

ol' :: Array ReactElement -> ReactElement
ol' = ol []

optgroup :: Array Props -> Array ReactElement -> ReactElement
optgroup = mkDOM (IsDynamic false) "optgroup"

optgroup' :: Array ReactElement -> ReactElement
optgroup' = optgroup []

option :: Array Props -> Array ReactElement -> ReactElement
option = mkDOM (IsDynamic false) "option"

option' :: Array ReactElement -> ReactElement
option' = option []

output :: Array Props -> Array ReactElement -> ReactElement
output = mkDOM (IsDynamic false) "output"

output' :: Array ReactElement -> ReactElement
output' = output []

p :: Array Props -> Array ReactElement -> ReactElement
p = mkDOM (IsDynamic false) "p"

p' :: Array ReactElement -> ReactElement
p' = p []

param :: Array Props -> Array ReactElement -> ReactElement
param = mkDOM (IsDynamic false) "param"

param' :: Array ReactElement -> ReactElement
param' = param []

picture :: Array Props -> Array ReactElement -> ReactElement
picture = mkDOM (IsDynamic false) "picture"

picture' :: Array ReactElement -> ReactElement
picture' = picture []

pre :: Array Props -> Array ReactElement -> ReactElement
pre = mkDOM (IsDynamic false) "pre"

pre' :: Array ReactElement -> ReactElement
pre' = pre []

progress :: Array Props -> Array ReactElement -> ReactElement
progress = mkDOM (IsDynamic false) "progress"

progress' :: Array ReactElement -> ReactElement
progress' = progress []

q :: Array Props -> Array ReactElement -> ReactElement
q = mkDOM (IsDynamic false) "q"

q' :: Array ReactElement -> ReactElement
q' = q []

rp :: Array Props -> Array ReactElement -> ReactElement
rp = mkDOM (IsDynamic false) "rp"

rp' :: Array ReactElement -> ReactElement
rp' = rp []

rt :: Array Props -> Array ReactElement -> ReactElement
rt = mkDOM (IsDynamic false) "rt"

rt' :: Array ReactElement -> ReactElement
rt' = rt []

ruby :: Array Props -> Array ReactElement -> ReactElement
ruby = mkDOM (IsDynamic false) "ruby"

ruby' :: Array ReactElement -> ReactElement
ruby' = ruby []

s :: Array Props -> Array ReactElement -> ReactElement
s = mkDOM (IsDynamic false) "s"

s' :: Array ReactElement -> ReactElement
s' = s []

samp :: Array Props -> Array ReactElement -> ReactElement
samp = mkDOM (IsDynamic false) "samp"

samp' :: Array ReactElement -> ReactElement
samp' = samp []

script :: Array Props -> Array ReactElement -> ReactElement
script = mkDOM (IsDynamic false) "script"

script' :: Array ReactElement -> ReactElement
script' = script []

section :: Array Props -> Array ReactElement -> ReactElement
section = mkDOM (IsDynamic false) "section"

section' :: Array ReactElement -> ReactElement
section' = section []

select :: Array Props -> Array ReactElement -> ReactElement
select = mkDOM (IsDynamic false) "select"

select' :: Array ReactElement -> ReactElement
select' = select []

small :: Array Props -> Array ReactElement -> ReactElement
small = mkDOM (IsDynamic false) "small"

small' :: Array ReactElement -> ReactElement
small' = small []

source :: Array Props -> Array ReactElement -> ReactElement
source = mkDOM (IsDynamic false) "source"

source' :: Array ReactElement -> ReactElement
source' = source []

span :: Array Props -> Array ReactElement -> ReactElement
span = mkDOM (IsDynamic false) "span"

span' :: Array ReactElement -> ReactElement
span' = span []

strong :: Array Props -> Array ReactElement -> ReactElement
strong = mkDOM (IsDynamic false) "strong"

strong' :: Array ReactElement -> ReactElement
strong' = strong []

style :: Array Props -> Array ReactElement -> ReactElement
style = mkDOM (IsDynamic false) "style"

style' :: Array ReactElement -> ReactElement
style' = style []

sub :: Array Props -> Array ReactElement -> ReactElement
sub = mkDOM (IsDynamic false) "sub"

sub' :: Array ReactElement -> ReactElement
sub' = sub []

summary :: Array Props -> Array ReactElement -> ReactElement
summary = mkDOM (IsDynamic false) "summary"

summary' :: Array ReactElement -> ReactElement
summary' = summary []

sup :: Array Props -> Array ReactElement -> ReactElement
sup = mkDOM (IsDynamic false) "sup"

sup' :: Array ReactElement -> ReactElement
sup' = sup []

table :: Array Props -> Array ReactElement -> ReactElement
table = mkDOM (IsDynamic false) "table"

table' :: Array ReactElement -> ReactElement
table' = table []

tbody :: Array Props -> Array ReactElement -> ReactElement
tbody = mkDOM (IsDynamic false) "tbody"

tbody' :: Array ReactElement -> ReactElement
tbody' = tbody []

td :: Array Props -> Array ReactElement -> ReactElement
td = mkDOM (IsDynamic false) "td"

td' :: Array ReactElement -> ReactElement
td' = td []

textarea :: Array Props -> Array ReactElement -> ReactElement
textarea = mkDOM (IsDynamic false) "textarea"

textarea' :: Array ReactElement -> ReactElement
textarea' = textarea []

tfoot :: Array Props -> Array ReactElement -> ReactElement
tfoot = mkDOM (IsDynamic false) "tfoot"

tfoot' :: Array ReactElement -> ReactElement
tfoot' = tfoot []

th :: Array Props -> Array ReactElement -> ReactElement
th = mkDOM (IsDynamic false) "th"

th' :: Array ReactElement -> ReactElement
th' = th []

thead :: Array Props -> Array ReactElement -> ReactElement
thead = mkDOM (IsDynamic false) "thead"

thead' :: Array ReactElement -> ReactElement
thead' = thead []

time :: Array Props -> Array ReactElement -> ReactElement
time = mkDOM (IsDynamic false) "time"

time' :: Array ReactElement -> ReactElement
time' = time []

title :: Array Props -> Array ReactElement -> ReactElement
title = mkDOM (IsDynamic false) "title"

title' :: Array ReactElement -> ReactElement
title' = title []

tr :: Array Props -> Array ReactElement -> ReactElement
tr = mkDOM (IsDynamic false) "tr"

tr' :: Array ReactElement -> ReactElement
tr' = tr []

track :: Array Props -> Array ReactElement -> ReactElement
track = mkDOM (IsDynamic false) "track"

track' :: Array ReactElement -> ReactElement
track' = track []

u :: Array Props -> Array ReactElement -> ReactElement
u = mkDOM (IsDynamic false) "u"

u' :: Array ReactElement -> ReactElement
u' = u []

ul :: Array Props -> Array ReactElement -> ReactElement
ul = mkDOM (IsDynamic false) "ul"

ul' :: Array ReactElement -> ReactElement
ul' = ul []

var :: Array Props -> Array ReactElement -> ReactElement
var = mkDOM (IsDynamic false) "var"

var' :: Array ReactElement -> ReactElement
var' = var []

video :: Array Props -> Array ReactElement -> ReactElement
video = mkDOM (IsDynamic false) "video"

video' :: Array ReactElement -> ReactElement
video' = video []

wbr :: Array Props -> Array ReactElement -> ReactElement
wbr = mkDOM (IsDynamic false) "body"

wbr' :: Array ReactElement -> ReactElement
wbr' = wbr []
