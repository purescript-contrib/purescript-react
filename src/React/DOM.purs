module React.DOM where

import Prelude hiding (sub, div, map)

import React
import React.DOM.Props (Props())

foreign import mkDOM :: String -> Array Props -> Array UI -> UI

foreign import text :: String -> UI

a :: Array Props -> Array UI -> UI
a = mkDOM "a"

a' :: Array UI -> UI
a' = a []

abbr :: Array Props -> Array UI -> UI
abbr = mkDOM "abbr"

abbr' :: Array UI -> UI
abbr' = abbr []

address :: Array Props -> Array UI -> UI
address = mkDOM "address"

address' :: Array UI -> UI
address' = address []

area :: Array Props -> Array UI -> UI
area = mkDOM "area"

area' :: Array UI -> UI
area' = area []

article :: Array Props -> Array UI -> UI
article = mkDOM "article"

article' :: Array UI -> UI
article' = article []

aside :: Array Props -> Array UI -> UI
aside = mkDOM "aside"

aside' :: Array UI -> UI
aside' = aside []

audio :: Array Props -> Array UI -> UI
audio = mkDOM "audio"

audio' :: Array UI -> UI
audio' = audio []

b :: Array Props -> Array UI -> UI
b = mkDOM "b"

b' :: Array UI -> UI
b' = b []

base :: Array Props -> Array UI -> UI
base = mkDOM "base"

base' :: Array UI -> UI
base' = base []

bdi :: Array Props -> Array UI -> UI
bdi = mkDOM "bdi"

bdi' :: Array UI -> UI
bdi' = bdi []

bdo :: Array Props -> Array UI -> UI
bdo = mkDOM "bdo"

bdo' :: Array UI -> UI
bdo' = bdo []

big :: Array Props -> Array UI -> UI
big = mkDOM "big"

big' :: Array UI -> UI
big' = big []

blockquote :: Array Props -> Array UI -> UI
blockquote = mkDOM "blockquote"

blockquote' :: Array UI -> UI
blockquote' = blockquote []

body :: Array Props -> Array UI -> UI
body = mkDOM "body"

body' :: Array UI -> UI
body' = body []

br :: Array Props -> Array UI -> UI
br = mkDOM "br"

br' :: Array UI -> UI
br' = br []

button :: Array Props -> Array UI -> UI
button = mkDOM "button"

button' :: Array UI -> UI
button' = button []

canvas :: Array Props -> Array UI -> UI
canvas = mkDOM "canvas"

canvas' :: Array UI -> UI
canvas' = canvas []

caption :: Array Props -> Array UI -> UI
caption = mkDOM "caption"

caption' :: Array UI -> UI
caption' = caption []

cite :: Array Props -> Array UI -> UI
cite = mkDOM "cite"

cite' :: Array UI -> UI
cite' = cite []

code :: Array Props -> Array UI -> UI
code = mkDOM "code"

code' :: Array UI -> UI
code' = code []

col :: Array Props -> Array UI -> UI
col = mkDOM "col"

col' :: Array UI -> UI
col' = col []

colgroup :: Array Props -> Array UI -> UI
colgroup = mkDOM "colgroup"

colgroup' :: Array UI -> UI
colgroup' = colgroup []

_data :: Array Props -> Array UI -> UI
_data = mkDOM "data"

_data' :: Array UI -> UI
_data' = _data []

datalist :: Array Props -> Array UI -> UI
datalist = mkDOM "datalist"

datalist' :: Array UI -> UI
datalist' = datalist []

dd :: Array Props -> Array UI -> UI
dd = mkDOM "dd"

dd' :: Array UI -> UI
dd' = dd []

del :: Array Props -> Array UI -> UI
del = mkDOM "del"

del' :: Array UI -> UI
del' = del []

details :: Array Props -> Array UI -> UI
details = mkDOM "details"

details' :: Array UI -> UI
details' = details []

dfn :: Array Props -> Array UI -> UI
dfn = mkDOM "dfn"

dfn' :: Array UI -> UI
dfn' = dfn []

dialog :: Array Props -> Array UI -> UI
dialog = mkDOM "dialog"

dialog' :: Array UI -> UI
dialog' = dialog []

div :: Array Props -> Array UI -> UI
div = mkDOM "div"

div' :: Array UI -> UI
div' = div []

dl :: Array Props -> Array UI -> UI
dl = mkDOM "dl"

dl' :: Array UI -> UI
dl' = dl []

dt :: Array Props -> Array UI -> UI
dt = mkDOM "dt"

dt' :: Array UI -> UI
dt' = dt []

em :: Array Props -> Array UI -> UI
em = mkDOM "em"

em' :: Array UI -> UI
em' = em []

embed :: Array Props -> Array UI -> UI
embed = mkDOM "embed"

embed' :: Array UI -> UI
embed' = embed []

fieldset :: Array Props -> Array UI -> UI
fieldset = mkDOM "fieldset"

fieldset' :: Array UI -> UI
fieldset' = fieldset []

figcaption :: Array Props -> Array UI -> UI
figcaption = mkDOM "figcaption"

figcaption' :: Array UI -> UI
figcaption' = figcaption []

figure :: Array Props -> Array UI -> UI
figure = mkDOM "figure"

figure' :: Array UI -> UI
figure' = figure []

footer :: Array Props -> Array UI -> UI
footer = mkDOM "footer"

footer' :: Array UI -> UI
footer' = footer []

form :: Array Props -> Array UI -> UI
form = mkDOM "form"

form' :: Array UI -> UI
form' = form []

h1 :: Array Props -> Array UI -> UI
h1 = mkDOM "h1"

h1' :: Array UI -> UI
h1' = h1 []

h2 :: Array Props -> Array UI -> UI
h2 = mkDOM "h2"

h2' :: Array UI -> UI
h2' = h2 []

h3 :: Array Props -> Array UI -> UI
h3 = mkDOM "h3"

h3' :: Array UI -> UI
h3' = h3 []

h4 :: Array Props -> Array UI -> UI
h4 = mkDOM "h4"

h4' :: Array UI -> UI
h4' = h4 []

h5 :: Array Props -> Array UI -> UI
h5 = mkDOM "h5"

h5' :: Array UI -> UI
h5' = h5 []

h6 :: Array Props -> Array UI -> UI
h6 = mkDOM "h6"

h6' :: Array UI -> UI
h6' = h6 []

head :: Array Props -> Array UI -> UI
head = mkDOM "head"

head' :: Array UI -> UI
head' = head []

header :: Array Props -> Array UI -> UI
header = mkDOM "header"

header' :: Array UI -> UI
header' = header []

hr :: Array Props -> Array UI -> UI
hr = mkDOM "hr"

hr' :: Array UI -> UI
hr' = hr []

html :: Array Props -> Array UI -> UI
html = mkDOM "html"

html' :: Array UI -> UI
html' = html []

i :: Array Props -> Array UI -> UI
i = mkDOM "i"

i' :: Array UI -> UI
i' = i []

iframe :: Array Props -> Array UI -> UI
iframe = mkDOM "iframe"

iframe' :: Array UI -> UI
iframe' = iframe []

img :: Array Props -> Array UI -> UI
img = mkDOM "img"

img' :: Array UI -> UI
img' = img []

input :: Array Props -> Array UI -> UI
input = mkDOM "input"

input' :: Array UI -> UI
input' = input []

ins :: Array Props -> Array UI -> UI
ins = mkDOM "ins"

ins' :: Array UI -> UI
ins' = ins []

kbd :: Array Props -> Array UI -> UI
kbd = mkDOM "kbd"

kbd' :: Array UI -> UI
kbd' = kbd []

keygen :: Array Props -> Array UI -> UI
keygen = mkDOM "keygen"

keygen' :: Array UI -> UI
keygen' = keygen []

label :: Array Props -> Array UI -> UI
label = mkDOM "label"

label' :: Array UI -> UI
label' = label []

legend :: Array Props -> Array UI -> UI
legend = mkDOM "legend"

legend' :: Array UI -> UI
legend' = legend []

li :: Array Props -> Array UI -> UI
li = mkDOM "li"

li' :: Array UI -> UI
li' = li []

link :: Array Props -> Array UI -> UI
link = mkDOM "link"

link' :: Array UI -> UI
link' = body []

main :: Array Props -> Array UI -> UI
main = mkDOM "main"

main' :: Array UI -> UI
main' = main []

map :: Array Props -> Array UI -> UI
map = mkDOM "map"

map' :: Array UI -> UI
map' = map []

mark :: Array Props -> Array UI -> UI
mark = mkDOM "mark"

mark' :: Array UI -> UI
mark' = mark []

menu :: Array Props -> Array UI -> UI
menu = mkDOM "menu"

menu' :: Array UI -> UI
menu' = menu []

menuitem :: Array Props -> Array UI -> UI
menuitem = mkDOM "menuitem"

menuitem' :: Array UI -> UI
menuitem' = menuitem []

meta :: Array Props -> Array UI -> UI
meta = mkDOM "meta"

meta' :: Array UI -> UI
meta' = meta []

meter :: Array Props -> Array UI -> UI
meter = mkDOM "meter"

meter' :: Array UI -> UI
meter' = meter []

nav :: Array Props -> Array UI -> UI
nav = mkDOM "nav"

nav' :: Array UI -> UI
nav' = nav []

noscript :: Array Props -> Array UI -> UI
noscript = mkDOM "noscript"

noscript' :: Array UI -> UI
noscript' = noscript []

object :: Array Props -> Array UI -> UI
object = mkDOM "object"

object' :: Array UI -> UI
object' = object []

ol :: Array Props -> Array UI -> UI
ol = mkDOM "ol"

ol' :: Array UI -> UI
ol' = ol []

optgroup :: Array Props -> Array UI -> UI
optgroup = mkDOM "optgroup"

optgroup' :: Array UI -> UI
optgroup' = optgroup []

option :: Array Props -> Array UI -> UI
option = mkDOM "option"

option' :: Array UI -> UI
option' = option []

output :: Array Props -> Array UI -> UI
output = mkDOM "output"

output' :: Array UI -> UI
output' = output []

p :: Array Props -> Array UI -> UI
p = mkDOM "p"

p' :: Array UI -> UI
p' = p []

param :: Array Props -> Array UI -> UI
param = mkDOM "param"

param' :: Array UI -> UI
param' = param []

picture :: Array Props -> Array UI -> UI
picture = mkDOM "picture"

picture' :: Array UI -> UI
picture' = picture []

pre :: Array Props -> Array UI -> UI
pre = mkDOM "pre"

pre' :: Array UI -> UI
pre' = pre []

progress :: Array Props -> Array UI -> UI
progress = mkDOM "progress"

progress' :: Array UI -> UI
progress' = progress []

q :: Array Props -> Array UI -> UI
q = mkDOM "q"

q' :: Array UI -> UI
q' = q []

rp :: Array Props -> Array UI -> UI
rp = mkDOM "rp"

rp' :: Array UI -> UI
rp' = rp []

rt :: Array Props -> Array UI -> UI
rt = mkDOM "rt"

rt' :: Array UI -> UI
rt' = rt []

ruby :: Array Props -> Array UI -> UI
ruby = mkDOM "ruby"

ruby' :: Array UI -> UI
ruby' = ruby []

s :: Array Props -> Array UI -> UI
s = mkDOM "s"

s' :: Array UI -> UI
s' = s []

samp :: Array Props -> Array UI -> UI
samp = mkDOM "samp"

samp' :: Array UI -> UI
samp' = samp []

script :: Array Props -> Array UI -> UI
script = mkDOM "script"

script' :: Array UI -> UI
script' = script []

section :: Array Props -> Array UI -> UI
section = mkDOM "section"

section' :: Array UI -> UI
section' = section []

select :: Array Props -> Array UI -> UI
select = mkDOM "select"

select' :: Array UI -> UI
select' = select []

small :: Array Props -> Array UI -> UI
small = mkDOM "small"

small' :: Array UI -> UI
small' = small []

source :: Array Props -> Array UI -> UI
source = mkDOM "source"

source' :: Array UI -> UI
source' = source []

span :: Array Props -> Array UI -> UI
span = mkDOM "span"

span' :: Array UI -> UI
span' = span []

strong :: Array Props -> Array UI -> UI
strong = mkDOM "strong"

strong' :: Array UI -> UI
strong' = strong []

style :: Array Props -> Array UI -> UI
style = mkDOM "style"

style' :: Array UI -> UI
style' = style []

sub :: Array Props -> Array UI -> UI
sub = mkDOM "sub"

sub' :: Array UI -> UI
sub' = sub []

summary :: Array Props -> Array UI -> UI
summary = mkDOM "summary"

summary' :: Array UI -> UI
summary' = summary []

sup :: Array Props -> Array UI -> UI
sup = mkDOM "sup"

sup' :: Array UI -> UI
sup' = sup []

table :: Array Props -> Array UI -> UI
table = mkDOM "table"

table' :: Array UI -> UI
table' = table []

tbody :: Array Props -> Array UI -> UI
tbody = mkDOM "tbody"

tbody' :: Array UI -> UI
tbody' = tbody []

td :: Array Props -> Array UI -> UI
td = mkDOM "td"

td' :: Array UI -> UI
td' = td []

textarea :: Array Props -> Array UI -> UI
textarea = mkDOM "textarea"

textarea' :: Array UI -> UI
textarea' = textarea []

tfoot :: Array Props -> Array UI -> UI
tfoot = mkDOM "tfoot"

tfoot' :: Array UI -> UI
tfoot' = tfoot []

th :: Array Props -> Array UI -> UI
th = mkDOM "th"

th' :: Array UI -> UI
th' = th []

thead :: Array Props -> Array UI -> UI
thead = mkDOM "thead"

thead' :: Array UI -> UI
thead' = thead []

time :: Array Props -> Array UI -> UI
time = mkDOM "time"

time' :: Array UI -> UI
time' = time []

title :: Array Props -> Array UI -> UI
title = mkDOM "title"

title' :: Array UI -> UI
title' = title []

tr :: Array Props -> Array UI -> UI
tr = mkDOM "tr"

tr' :: Array UI -> UI
tr' = tr []

track :: Array Props -> Array UI -> UI
track = mkDOM "track"

track' :: Array UI -> UI
track' = track []

u :: Array Props -> Array UI -> UI
u = mkDOM "u"

u' :: Array UI -> UI
u' = u []

ul :: Array Props -> Array UI -> UI
ul = mkDOM "ul"

ul' :: Array UI -> UI
ul' = ul []

var :: Array Props -> Array UI -> UI
var = mkDOM "var"

var' :: Array UI -> UI
var' = var []

video :: Array Props -> Array UI -> UI
video = mkDOM "video"

video' :: Array UI -> UI
video' = video []

wbr :: Array Props -> Array UI -> UI
wbr = mkDOM "body"

wbr' :: Array UI -> UI
wbr' = wbr []
