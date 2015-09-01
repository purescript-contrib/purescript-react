module React.DOM where

import Prelude hiding (sub, div, map)

import React
import React.DOM.Props (Props())

foreign import mkDOM :: String -> Array Props -> Array ReactElement -> ReactElement

foreign import text :: String -> ReactElement

a :: Array Props -> Array ReactElement -> ReactElement
a = mkDOM "a"

a' :: Array ReactElement -> ReactElement
a' = a []

abbr :: Array Props -> Array ReactElement -> ReactElement
abbr = mkDOM "abbr"

abbr' :: Array ReactElement -> ReactElement
abbr' = abbr []

address :: Array Props -> Array ReactElement -> ReactElement
address = mkDOM "address"

address' :: Array ReactElement -> ReactElement
address' = address []

area :: Array Props -> Array ReactElement -> ReactElement
area = mkDOM "area"

area' :: Array ReactElement -> ReactElement
area' = area []

article :: Array Props -> Array ReactElement -> ReactElement
article = mkDOM "article"

article' :: Array ReactElement -> ReactElement
article' = article []

aside :: Array Props -> Array ReactElement -> ReactElement
aside = mkDOM "aside"

aside' :: Array ReactElement -> ReactElement
aside' = aside []

audio :: Array Props -> Array ReactElement -> ReactElement
audio = mkDOM "audio"

audio' :: Array ReactElement -> ReactElement
audio' = audio []

b :: Array Props -> Array ReactElement -> ReactElement
b = mkDOM "b"

b' :: Array ReactElement -> ReactElement
b' = b []

base :: Array Props -> Array ReactElement -> ReactElement
base = mkDOM "base"

base' :: Array ReactElement -> ReactElement
base' = base []

bdi :: Array Props -> Array ReactElement -> ReactElement
bdi = mkDOM "bdi"

bdi' :: Array ReactElement -> ReactElement
bdi' = bdi []

bdo :: Array Props -> Array ReactElement -> ReactElement
bdo = mkDOM "bdo"

bdo' :: Array ReactElement -> ReactElement
bdo' = bdo []

big :: Array Props -> Array ReactElement -> ReactElement
big = mkDOM "big"

big' :: Array ReactElement -> ReactElement
big' = big []

blockquote :: Array Props -> Array ReactElement -> ReactElement
blockquote = mkDOM "blockquote"

blockquote' :: Array ReactElement -> ReactElement
blockquote' = blockquote []

body :: Array Props -> Array ReactElement -> ReactElement
body = mkDOM "body"

body' :: Array ReactElement -> ReactElement
body' = body []

br :: Array Props -> Array ReactElement -> ReactElement
br = mkDOM "br"

br' :: Array ReactElement -> ReactElement
br' = br []

button :: Array Props -> Array ReactElement -> ReactElement
button = mkDOM "button"

button' :: Array ReactElement -> ReactElement
button' = button []

canvas :: Array Props -> Array ReactElement -> ReactElement
canvas = mkDOM "canvas"

canvas' :: Array ReactElement -> ReactElement
canvas' = canvas []

caption :: Array Props -> Array ReactElement -> ReactElement
caption = mkDOM "caption"

caption' :: Array ReactElement -> ReactElement
caption' = caption []

cite :: Array Props -> Array ReactElement -> ReactElement
cite = mkDOM "cite"

cite' :: Array ReactElement -> ReactElement
cite' = cite []

code :: Array Props -> Array ReactElement -> ReactElement
code = mkDOM "code"

code' :: Array ReactElement -> ReactElement
code' = code []

col :: Array Props -> Array ReactElement -> ReactElement
col = mkDOM "col"

col' :: Array ReactElement -> ReactElement
col' = col []

colgroup :: Array Props -> Array ReactElement -> ReactElement
colgroup = mkDOM "colgroup"

colgroup' :: Array ReactElement -> ReactElement
colgroup' = colgroup []

_data :: Array Props -> Array ReactElement -> ReactElement
_data = mkDOM "data"

_data' :: Array ReactElement -> ReactElement
_data' = _data []

datalist :: Array Props -> Array ReactElement -> ReactElement
datalist = mkDOM "datalist"

datalist' :: Array ReactElement -> ReactElement
datalist' = datalist []

dd :: Array Props -> Array ReactElement -> ReactElement
dd = mkDOM "dd"

dd' :: Array ReactElement -> ReactElement
dd' = dd []

del :: Array Props -> Array ReactElement -> ReactElement
del = mkDOM "del"

del' :: Array ReactElement -> ReactElement
del' = del []

details :: Array Props -> Array ReactElement -> ReactElement
details = mkDOM "details"

details' :: Array ReactElement -> ReactElement
details' = details []

dfn :: Array Props -> Array ReactElement -> ReactElement
dfn = mkDOM "dfn"

dfn' :: Array ReactElement -> ReactElement
dfn' = dfn []

dialog :: Array Props -> Array ReactElement -> ReactElement
dialog = mkDOM "dialog"

dialog' :: Array ReactElement -> ReactElement
dialog' = dialog []

div :: Array Props -> Array ReactElement -> ReactElement
div = mkDOM "div"

div' :: Array ReactElement -> ReactElement
div' = div []

dl :: Array Props -> Array ReactElement -> ReactElement
dl = mkDOM "dl"

dl' :: Array ReactElement -> ReactElement
dl' = dl []

dt :: Array Props -> Array ReactElement -> ReactElement
dt = mkDOM "dt"

dt' :: Array ReactElement -> ReactElement
dt' = dt []

em :: Array Props -> Array ReactElement -> ReactElement
em = mkDOM "em"

em' :: Array ReactElement -> ReactElement
em' = em []

embed :: Array Props -> Array ReactElement -> ReactElement
embed = mkDOM "embed"

embed' :: Array ReactElement -> ReactElement
embed' = embed []

fieldset :: Array Props -> Array ReactElement -> ReactElement
fieldset = mkDOM "fieldset"

fieldset' :: Array ReactElement -> ReactElement
fieldset' = fieldset []

figcaption :: Array Props -> Array ReactElement -> ReactElement
figcaption = mkDOM "figcaption"

figcaption' :: Array ReactElement -> ReactElement
figcaption' = figcaption []

figure :: Array Props -> Array ReactElement -> ReactElement
figure = mkDOM "figure"

figure' :: Array ReactElement -> ReactElement
figure' = figure []

footer :: Array Props -> Array ReactElement -> ReactElement
footer = mkDOM "footer"

footer' :: Array ReactElement -> ReactElement
footer' = footer []

form :: Array Props -> Array ReactElement -> ReactElement
form = mkDOM "form"

form' :: Array ReactElement -> ReactElement
form' = form []

h1 :: Array Props -> Array ReactElement -> ReactElement
h1 = mkDOM "h1"

h1' :: Array ReactElement -> ReactElement
h1' = h1 []

h2 :: Array Props -> Array ReactElement -> ReactElement
h2 = mkDOM "h2"

h2' :: Array ReactElement -> ReactElement
h2' = h2 []

h3 :: Array Props -> Array ReactElement -> ReactElement
h3 = mkDOM "h3"

h3' :: Array ReactElement -> ReactElement
h3' = h3 []

h4 :: Array Props -> Array ReactElement -> ReactElement
h4 = mkDOM "h4"

h4' :: Array ReactElement -> ReactElement
h4' = h4 []

h5 :: Array Props -> Array ReactElement -> ReactElement
h5 = mkDOM "h5"

h5' :: Array ReactElement -> ReactElement
h5' = h5 []

h6 :: Array Props -> Array ReactElement -> ReactElement
h6 = mkDOM "h6"

h6' :: Array ReactElement -> ReactElement
h6' = h6 []

head :: Array Props -> Array ReactElement -> ReactElement
head = mkDOM "head"

head' :: Array ReactElement -> ReactElement
head' = head []

header :: Array Props -> Array ReactElement -> ReactElement
header = mkDOM "header"

header' :: Array ReactElement -> ReactElement
header' = header []

hr :: Array Props -> Array ReactElement -> ReactElement
hr = mkDOM "hr"

hr' :: Array ReactElement -> ReactElement
hr' = hr []

html :: Array Props -> Array ReactElement -> ReactElement
html = mkDOM "html"

html' :: Array ReactElement -> ReactElement
html' = html []

i :: Array Props -> Array ReactElement -> ReactElement
i = mkDOM "i"

i' :: Array ReactElement -> ReactElement
i' = i []

iframe :: Array Props -> Array ReactElement -> ReactElement
iframe = mkDOM "iframe"

iframe' :: Array ReactElement -> ReactElement
iframe' = iframe []

img :: Array Props -> Array ReactElement -> ReactElement
img = mkDOM "img"

img' :: Array ReactElement -> ReactElement
img' = img []

input :: Array Props -> Array ReactElement -> ReactElement
input = mkDOM "input"

input' :: Array ReactElement -> ReactElement
input' = input []

ins :: Array Props -> Array ReactElement -> ReactElement
ins = mkDOM "ins"

ins' :: Array ReactElement -> ReactElement
ins' = ins []

kbd :: Array Props -> Array ReactElement -> ReactElement
kbd = mkDOM "kbd"

kbd' :: Array ReactElement -> ReactElement
kbd' = kbd []

keygen :: Array Props -> Array ReactElement -> ReactElement
keygen = mkDOM "keygen"

keygen' :: Array ReactElement -> ReactElement
keygen' = keygen []

label :: Array Props -> Array ReactElement -> ReactElement
label = mkDOM "label"

label' :: Array ReactElement -> ReactElement
label' = label []

legend :: Array Props -> Array ReactElement -> ReactElement
legend = mkDOM "legend"

legend' :: Array ReactElement -> ReactElement
legend' = legend []

li :: Array Props -> Array ReactElement -> ReactElement
li = mkDOM "li"

li' :: Array ReactElement -> ReactElement
li' = li []

link :: Array Props -> Array ReactElement -> ReactElement
link = mkDOM "link"

link' :: Array ReactElement -> ReactElement
link' = body []

main :: Array Props -> Array ReactElement -> ReactElement
main = mkDOM "main"

main' :: Array ReactElement -> ReactElement
main' = main []

map :: Array Props -> Array ReactElement -> ReactElement
map = mkDOM "map"

map' :: Array ReactElement -> ReactElement
map' = map []

mark :: Array Props -> Array ReactElement -> ReactElement
mark = mkDOM "mark"

mark' :: Array ReactElement -> ReactElement
mark' = mark []

menu :: Array Props -> Array ReactElement -> ReactElement
menu = mkDOM "menu"

menu' :: Array ReactElement -> ReactElement
menu' = menu []

menuitem :: Array Props -> Array ReactElement -> ReactElement
menuitem = mkDOM "menuitem"

menuitem' :: Array ReactElement -> ReactElement
menuitem' = menuitem []

meta :: Array Props -> Array ReactElement -> ReactElement
meta = mkDOM "meta"

meta' :: Array ReactElement -> ReactElement
meta' = meta []

meter :: Array Props -> Array ReactElement -> ReactElement
meter = mkDOM "meter"

meter' :: Array ReactElement -> ReactElement
meter' = meter []

nav :: Array Props -> Array ReactElement -> ReactElement
nav = mkDOM "nav"

nav' :: Array ReactElement -> ReactElement
nav' = nav []

noscript :: Array Props -> Array ReactElement -> ReactElement
noscript = mkDOM "noscript"

noscript' :: Array ReactElement -> ReactElement
noscript' = noscript []

object :: Array Props -> Array ReactElement -> ReactElement
object = mkDOM "object"

object' :: Array ReactElement -> ReactElement
object' = object []

ol :: Array Props -> Array ReactElement -> ReactElement
ol = mkDOM "ol"

ol' :: Array ReactElement -> ReactElement
ol' = ol []

optgroup :: Array Props -> Array ReactElement -> ReactElement
optgroup = mkDOM "optgroup"

optgroup' :: Array ReactElement -> ReactElement
optgroup' = optgroup []

option :: Array Props -> Array ReactElement -> ReactElement
option = mkDOM "option"

option' :: Array ReactElement -> ReactElement
option' = option []

output :: Array Props -> Array ReactElement -> ReactElement
output = mkDOM "output"

output' :: Array ReactElement -> ReactElement
output' = output []

p :: Array Props -> Array ReactElement -> ReactElement
p = mkDOM "p"

p' :: Array ReactElement -> ReactElement
p' = p []

param :: Array Props -> Array ReactElement -> ReactElement
param = mkDOM "param"

param' :: Array ReactElement -> ReactElement
param' = param []

picture :: Array Props -> Array ReactElement -> ReactElement
picture = mkDOM "picture"

picture' :: Array ReactElement -> ReactElement
picture' = picture []

pre :: Array Props -> Array ReactElement -> ReactElement
pre = mkDOM "pre"

pre' :: Array ReactElement -> ReactElement
pre' = pre []

progress :: Array Props -> Array ReactElement -> ReactElement
progress = mkDOM "progress"

progress' :: Array ReactElement -> ReactElement
progress' = progress []

q :: Array Props -> Array ReactElement -> ReactElement
q = mkDOM "q"

q' :: Array ReactElement -> ReactElement
q' = q []

rp :: Array Props -> Array ReactElement -> ReactElement
rp = mkDOM "rp"

rp' :: Array ReactElement -> ReactElement
rp' = rp []

rt :: Array Props -> Array ReactElement -> ReactElement
rt = mkDOM "rt"

rt' :: Array ReactElement -> ReactElement
rt' = rt []

ruby :: Array Props -> Array ReactElement -> ReactElement
ruby = mkDOM "ruby"

ruby' :: Array ReactElement -> ReactElement
ruby' = ruby []

s :: Array Props -> Array ReactElement -> ReactElement
s = mkDOM "s"

s' :: Array ReactElement -> ReactElement
s' = s []

samp :: Array Props -> Array ReactElement -> ReactElement
samp = mkDOM "samp"

samp' :: Array ReactElement -> ReactElement
samp' = samp []

script :: Array Props -> Array ReactElement -> ReactElement
script = mkDOM "script"

script' :: Array ReactElement -> ReactElement
script' = script []

section :: Array Props -> Array ReactElement -> ReactElement
section = mkDOM "section"

section' :: Array ReactElement -> ReactElement
section' = section []

select :: Array Props -> Array ReactElement -> ReactElement
select = mkDOM "select"

select' :: Array ReactElement -> ReactElement
select' = select []

small :: Array Props -> Array ReactElement -> ReactElement
small = mkDOM "small"

small' :: Array ReactElement -> ReactElement
small' = small []

source :: Array Props -> Array ReactElement -> ReactElement
source = mkDOM "source"

source' :: Array ReactElement -> ReactElement
source' = source []

span :: Array Props -> Array ReactElement -> ReactElement
span = mkDOM "span"

span' :: Array ReactElement -> ReactElement
span' = span []

strong :: Array Props -> Array ReactElement -> ReactElement
strong = mkDOM "strong"

strong' :: Array ReactElement -> ReactElement
strong' = strong []

style :: Array Props -> Array ReactElement -> ReactElement
style = mkDOM "style"

style' :: Array ReactElement -> ReactElement
style' = style []

sub :: Array Props -> Array ReactElement -> ReactElement
sub = mkDOM "sub"

sub' :: Array ReactElement -> ReactElement
sub' = sub []

summary :: Array Props -> Array ReactElement -> ReactElement
summary = mkDOM "summary"

summary' :: Array ReactElement -> ReactElement
summary' = summary []

sup :: Array Props -> Array ReactElement -> ReactElement
sup = mkDOM "sup"

sup' :: Array ReactElement -> ReactElement
sup' = sup []

table :: Array Props -> Array ReactElement -> ReactElement
table = mkDOM "table"

table' :: Array ReactElement -> ReactElement
table' = table []

tbody :: Array Props -> Array ReactElement -> ReactElement
tbody = mkDOM "tbody"

tbody' :: Array ReactElement -> ReactElement
tbody' = tbody []

td :: Array Props -> Array ReactElement -> ReactElement
td = mkDOM "td"

td' :: Array ReactElement -> ReactElement
td' = td []

textarea :: Array Props -> Array ReactElement -> ReactElement
textarea = mkDOM "textarea"

textarea' :: Array ReactElement -> ReactElement
textarea' = textarea []

tfoot :: Array Props -> Array ReactElement -> ReactElement
tfoot = mkDOM "tfoot"

tfoot' :: Array ReactElement -> ReactElement
tfoot' = tfoot []

th :: Array Props -> Array ReactElement -> ReactElement
th = mkDOM "th"

th' :: Array ReactElement -> ReactElement
th' = th []

thead :: Array Props -> Array ReactElement -> ReactElement
thead = mkDOM "thead"

thead' :: Array ReactElement -> ReactElement
thead' = thead []

time :: Array Props -> Array ReactElement -> ReactElement
time = mkDOM "time"

time' :: Array ReactElement -> ReactElement
time' = time []

title :: Array Props -> Array ReactElement -> ReactElement
title = mkDOM "title"

title' :: Array ReactElement -> ReactElement
title' = title []

tr :: Array Props -> Array ReactElement -> ReactElement
tr = mkDOM "tr"

tr' :: Array ReactElement -> ReactElement
tr' = tr []

track :: Array Props -> Array ReactElement -> ReactElement
track = mkDOM "track"

track' :: Array ReactElement -> ReactElement
track' = track []

u :: Array Props -> Array ReactElement -> ReactElement
u = mkDOM "u"

u' :: Array ReactElement -> ReactElement
u' = u []

ul :: Array Props -> Array ReactElement -> ReactElement
ul = mkDOM "ul"

ul' :: Array ReactElement -> ReactElement
ul' = ul []

var :: Array Props -> Array ReactElement -> ReactElement
var = mkDOM "var"

var' :: Array ReactElement -> ReactElement
var' = var []

video :: Array Props -> Array ReactElement -> ReactElement
video = mkDOM "video"

video' :: Array ReactElement -> ReactElement
video' = video []

wbr :: Array Props -> Array ReactElement -> ReactElement
wbr = mkDOM "body"

wbr' :: Array ReactElement -> ReactElement
wbr' = wbr []
