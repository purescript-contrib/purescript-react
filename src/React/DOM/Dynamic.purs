module React.DOM.Dynamic where

import React (ReactElement())
import React.DOM.Props (Props())
import qualified React.DOM as DOM

text :: String -> ReactElement
text = DOM.text

a :: Array Props -> Array ReactElement -> ReactElement
a = DOM.mkDOM (DOM.IsDynamic true) "a"

a' :: Array ReactElement -> ReactElement
a' = a []

abbr :: Array Props -> Array ReactElement -> ReactElement
abbr = DOM.mkDOM (DOM.IsDynamic true) "abbr"

abbr' :: Array ReactElement -> ReactElement
abbr' = abbr []

address :: Array Props -> Array ReactElement -> ReactElement
address = DOM.mkDOM (DOM.IsDynamic true) "address"

address' :: Array ReactElement -> ReactElement
address' = address []

area :: Array Props -> Array ReactElement -> ReactElement
area = DOM.mkDOM (DOM.IsDynamic true) "area"

area' :: Array ReactElement -> ReactElement
area' = area []

article :: Array Props -> Array ReactElement -> ReactElement
article = DOM.mkDOM (DOM.IsDynamic true) "article"

article' :: Array ReactElement -> ReactElement
article' = article []

aside :: Array Props -> Array ReactElement -> ReactElement
aside = DOM.mkDOM (DOM.IsDynamic true) "aside"

aside' :: Array ReactElement -> ReactElement
aside' = aside []

audio :: Array Props -> Array ReactElement -> ReactElement
audio = DOM.mkDOM (DOM.IsDynamic true) "audio"

audio' :: Array ReactElement -> ReactElement
audio' = audio []

b :: Array Props -> Array ReactElement -> ReactElement
b = DOM.mkDOM (DOM.IsDynamic true) "b"

b' :: Array ReactElement -> ReactElement
b' = b []

base :: Array Props -> Array ReactElement -> ReactElement
base = DOM.mkDOM (DOM.IsDynamic true) "base"

base' :: Array ReactElement -> ReactElement
base' = base []

bdi :: Array Props -> Array ReactElement -> ReactElement
bdi = DOM.mkDOM (DOM.IsDynamic true) "bdi"

bdi' :: Array ReactElement -> ReactElement
bdi' = bdi []

bdo :: Array Props -> Array ReactElement -> ReactElement
bdo = DOM.mkDOM (DOM.IsDynamic true) "bdo"

bdo' :: Array ReactElement -> ReactElement
bdo' = bdo []

big :: Array Props -> Array ReactElement -> ReactElement
big = DOM.mkDOM (DOM.IsDynamic true) "big"

big' :: Array ReactElement -> ReactElement
big' = big []

blockquote :: Array Props -> Array ReactElement -> ReactElement
blockquote = DOM.mkDOM (DOM.IsDynamic true) "blockquote"

blockquote' :: Array ReactElement -> ReactElement
blockquote' = blockquote []

body :: Array Props -> Array ReactElement -> ReactElement
body = DOM.mkDOM (DOM.IsDynamic true) "body"

body' :: Array ReactElement -> ReactElement
body' = body []

br :: Array Props -> Array ReactElement -> ReactElement
br = DOM.mkDOM (DOM.IsDynamic true) "br"

br' :: Array ReactElement -> ReactElement
br' = br []

button :: Array Props -> Array ReactElement -> ReactElement
button = DOM.mkDOM (DOM.IsDynamic true) "button"

button' :: Array ReactElement -> ReactElement
button' = button []

canvas :: Array Props -> Array ReactElement -> ReactElement
canvas = DOM.mkDOM (DOM.IsDynamic true) "canvas"

canvas' :: Array ReactElement -> ReactElement
canvas' = canvas []

caption :: Array Props -> Array ReactElement -> ReactElement
caption = DOM.mkDOM (DOM.IsDynamic true) "caption"

caption' :: Array ReactElement -> ReactElement
caption' = caption []

cite :: Array Props -> Array ReactElement -> ReactElement
cite = DOM.mkDOM (DOM.IsDynamic true) "cite"

cite' :: Array ReactElement -> ReactElement
cite' = cite []

code :: Array Props -> Array ReactElement -> ReactElement
code = DOM.mkDOM (DOM.IsDynamic true) "code"

code' :: Array ReactElement -> ReactElement
code' = code []

col :: Array Props -> Array ReactElement -> ReactElement
col = DOM.mkDOM (DOM.IsDynamic true) "col"

col' :: Array ReactElement -> ReactElement
col' = col []

colgroup :: Array Props -> Array ReactElement -> ReactElement
colgroup = DOM.mkDOM (DOM.IsDynamic true) "colgroup"

colgroup' :: Array ReactElement -> ReactElement
colgroup' = colgroup []

_data :: Array Props -> Array ReactElement -> ReactElement
_data = DOM.mkDOM (DOM.IsDynamic true) "data"

_data' :: Array ReactElement -> ReactElement
_data' = _data []

datalist :: Array Props -> Array ReactElement -> ReactElement
datalist = DOM.mkDOM (DOM.IsDynamic true) "datalist"

datalist' :: Array ReactElement -> ReactElement
datalist' = datalist []

dd :: Array Props -> Array ReactElement -> ReactElement
dd = DOM.mkDOM (DOM.IsDynamic true) "dd"

dd' :: Array ReactElement -> ReactElement
dd' = dd []

del :: Array Props -> Array ReactElement -> ReactElement
del = DOM.mkDOM (DOM.IsDynamic true) "del"

del' :: Array ReactElement -> ReactElement
del' = del []

details :: Array Props -> Array ReactElement -> ReactElement
details = DOM.mkDOM (DOM.IsDynamic true) "details"

details' :: Array ReactElement -> ReactElement
details' = details []

dfn :: Array Props -> Array ReactElement -> ReactElement
dfn = DOM.mkDOM (DOM.IsDynamic true) "dfn"

dfn' :: Array ReactElement -> ReactElement
dfn' = dfn []

dialog :: Array Props -> Array ReactElement -> ReactElement
dialog = DOM.mkDOM (DOM.IsDynamic true) "dialog"

dialog' :: Array ReactElement -> ReactElement
dialog' = dialog []

div :: Array Props -> Array ReactElement -> ReactElement
div = DOM.mkDOM (DOM.IsDynamic true) "div"

div' :: Array ReactElement -> ReactElement
div' = div []

dl :: Array Props -> Array ReactElement -> ReactElement
dl = DOM.mkDOM (DOM.IsDynamic true) "dl"

dl' :: Array ReactElement -> ReactElement
dl' = dl []

dt :: Array Props -> Array ReactElement -> ReactElement
dt = DOM.mkDOM (DOM.IsDynamic true) "dt"

dt' :: Array ReactElement -> ReactElement
dt' = dt []

em :: Array Props -> Array ReactElement -> ReactElement
em = DOM.mkDOM (DOM.IsDynamic true) "em"

em' :: Array ReactElement -> ReactElement
em' = em []

embed :: Array Props -> Array ReactElement -> ReactElement
embed = DOM.mkDOM (DOM.IsDynamic true) "embed"

embed' :: Array ReactElement -> ReactElement
embed' = embed []

fieldset :: Array Props -> Array ReactElement -> ReactElement
fieldset = DOM.mkDOM (DOM.IsDynamic true) "fieldset"

fieldset' :: Array ReactElement -> ReactElement
fieldset' = fieldset []

figcaption :: Array Props -> Array ReactElement -> ReactElement
figcaption = DOM.mkDOM (DOM.IsDynamic true) "figcaption"

figcaption' :: Array ReactElement -> ReactElement
figcaption' = figcaption []

figure :: Array Props -> Array ReactElement -> ReactElement
figure = DOM.mkDOM (DOM.IsDynamic true) "figure"

figure' :: Array ReactElement -> ReactElement
figure' = figure []

footer :: Array Props -> Array ReactElement -> ReactElement
footer = DOM.mkDOM (DOM.IsDynamic true) "footer"

footer' :: Array ReactElement -> ReactElement
footer' = footer []

form :: Array Props -> Array ReactElement -> ReactElement
form = DOM.mkDOM (DOM.IsDynamic true) "form"

form' :: Array ReactElement -> ReactElement
form' = form []

h1 :: Array Props -> Array ReactElement -> ReactElement
h1 = DOM.mkDOM (DOM.IsDynamic true) "h1"

h1' :: Array ReactElement -> ReactElement
h1' = h1 []

h2 :: Array Props -> Array ReactElement -> ReactElement
h2 = DOM.mkDOM (DOM.IsDynamic true) "h2"

h2' :: Array ReactElement -> ReactElement
h2' = h2 []

h3 :: Array Props -> Array ReactElement -> ReactElement
h3 = DOM.mkDOM (DOM.IsDynamic true) "h3"

h3' :: Array ReactElement -> ReactElement
h3' = h3 []

h4 :: Array Props -> Array ReactElement -> ReactElement
h4 = DOM.mkDOM (DOM.IsDynamic true) "h4"

h4' :: Array ReactElement -> ReactElement
h4' = h4 []

h5 :: Array Props -> Array ReactElement -> ReactElement
h5 = DOM.mkDOM (DOM.IsDynamic true) "h5"

h5' :: Array ReactElement -> ReactElement
h5' = h5 []

h6 :: Array Props -> Array ReactElement -> ReactElement
h6 = DOM.mkDOM (DOM.IsDynamic true) "h6"

h6' :: Array ReactElement -> ReactElement
h6' = h6 []

head :: Array Props -> Array ReactElement -> ReactElement
head = DOM.mkDOM (DOM.IsDynamic true) "head"

head' :: Array ReactElement -> ReactElement
head' = head []

header :: Array Props -> Array ReactElement -> ReactElement
header = DOM.mkDOM (DOM.IsDynamic true) "header"

header' :: Array ReactElement -> ReactElement
header' = header []

hr :: Array Props -> Array ReactElement -> ReactElement
hr = DOM.mkDOM (DOM.IsDynamic true) "hr"

hr' :: Array ReactElement -> ReactElement
hr' = hr []

html :: Array Props -> Array ReactElement -> ReactElement
html = DOM.mkDOM (DOM.IsDynamic true) "html"

html' :: Array ReactElement -> ReactElement
html' = html []

i :: Array Props -> Array ReactElement -> ReactElement
i = DOM.mkDOM (DOM.IsDynamic true) "i"

i' :: Array ReactElement -> ReactElement
i' = i []

iframe :: Array Props -> Array ReactElement -> ReactElement
iframe = DOM.mkDOM (DOM.IsDynamic true) "iframe"

iframe' :: Array ReactElement -> ReactElement
iframe' = iframe []

img :: Array Props -> Array ReactElement -> ReactElement
img = DOM.mkDOM (DOM.IsDynamic true) "img"

img' :: Array ReactElement -> ReactElement
img' = img []

input :: Array Props -> Array ReactElement -> ReactElement
input = DOM.mkDOM (DOM.IsDynamic true) "input"

input' :: Array ReactElement -> ReactElement
input' = input []

ins :: Array Props -> Array ReactElement -> ReactElement
ins = DOM.mkDOM (DOM.IsDynamic true) "ins"

ins' :: Array ReactElement -> ReactElement
ins' = ins []

kbd :: Array Props -> Array ReactElement -> ReactElement
kbd = DOM.mkDOM (DOM.IsDynamic true) "kbd"

kbd' :: Array ReactElement -> ReactElement
kbd' = kbd []

keygen :: Array Props -> Array ReactElement -> ReactElement
keygen = DOM.mkDOM (DOM.IsDynamic true) "keygen"

keygen' :: Array ReactElement -> ReactElement
keygen' = keygen []

label :: Array Props -> Array ReactElement -> ReactElement
label = DOM.mkDOM (DOM.IsDynamic true) "label"

label' :: Array ReactElement -> ReactElement
label' = label []

legend :: Array Props -> Array ReactElement -> ReactElement
legend = DOM.mkDOM (DOM.IsDynamic true) "legend"

legend' :: Array ReactElement -> ReactElement
legend' = legend []

li :: Array Props -> Array ReactElement -> ReactElement
li = DOM.mkDOM (DOM.IsDynamic true) "li"

li' :: Array ReactElement -> ReactElement
li' = li []

link :: Array Props -> Array ReactElement -> ReactElement
link = DOM.mkDOM (DOM.IsDynamic true) "link"

link' :: Array ReactElement -> ReactElement
link' = body []

main :: Array Props -> Array ReactElement -> ReactElement
main = DOM.mkDOM (DOM.IsDynamic true) "main"

main' :: Array ReactElement -> ReactElement
main' = main []

map :: Array Props -> Array ReactElement -> ReactElement
map = DOM.mkDOM (DOM.IsDynamic true) "map"

map' :: Array ReactElement -> ReactElement
map' = map []

mark :: Array Props -> Array ReactElement -> ReactElement
mark = DOM.mkDOM (DOM.IsDynamic true) "mark"

mark' :: Array ReactElement -> ReactElement
mark' = mark []

menu :: Array Props -> Array ReactElement -> ReactElement
menu = DOM.mkDOM (DOM.IsDynamic true) "menu"

menu' :: Array ReactElement -> ReactElement
menu' = menu []

menuitem :: Array Props -> Array ReactElement -> ReactElement
menuitem = DOM.mkDOM (DOM.IsDynamic true) "menuitem"

menuitem' :: Array ReactElement -> ReactElement
menuitem' = menuitem []

meta :: Array Props -> Array ReactElement -> ReactElement
meta = DOM.mkDOM (DOM.IsDynamic true) "meta"

meta' :: Array ReactElement -> ReactElement
meta' = meta []

meter :: Array Props -> Array ReactElement -> ReactElement
meter = DOM.mkDOM (DOM.IsDynamic true) "meter"

meter' :: Array ReactElement -> ReactElement
meter' = meter []

nav :: Array Props -> Array ReactElement -> ReactElement
nav = DOM.mkDOM (DOM.IsDynamic true) "nav"

nav' :: Array ReactElement -> ReactElement
nav' = nav []

noscript :: Array Props -> Array ReactElement -> ReactElement
noscript = DOM.mkDOM (DOM.IsDynamic true) "noscript"

noscript' :: Array ReactElement -> ReactElement
noscript' = noscript []

object :: Array Props -> Array ReactElement -> ReactElement
object = DOM.mkDOM (DOM.IsDynamic true) "object"

object' :: Array ReactElement -> ReactElement
object' = object []

ol :: Array Props -> Array ReactElement -> ReactElement
ol = DOM.mkDOM (DOM.IsDynamic true) "ol"

ol' :: Array ReactElement -> ReactElement
ol' = ol []

optgroup :: Array Props -> Array ReactElement -> ReactElement
optgroup = DOM.mkDOM (DOM.IsDynamic true) "optgroup"

optgroup' :: Array ReactElement -> ReactElement
optgroup' = optgroup []

option :: Array Props -> Array ReactElement -> ReactElement
option = DOM.mkDOM (DOM.IsDynamic true) "option"

option' :: Array ReactElement -> ReactElement
option' = option []

output :: Array Props -> Array ReactElement -> ReactElement
output = DOM.mkDOM (DOM.IsDynamic true) "output"

output' :: Array ReactElement -> ReactElement
output' = output []

p :: Array Props -> Array ReactElement -> ReactElement
p = DOM.mkDOM (DOM.IsDynamic true) "p"

p' :: Array ReactElement -> ReactElement
p' = p []

param :: Array Props -> Array ReactElement -> ReactElement
param = DOM.mkDOM (DOM.IsDynamic true) "param"

param' :: Array ReactElement -> ReactElement
param' = param []

picture :: Array Props -> Array ReactElement -> ReactElement
picture = DOM.mkDOM (DOM.IsDynamic true) "picture"

picture' :: Array ReactElement -> ReactElement
picture' = picture []

pre :: Array Props -> Array ReactElement -> ReactElement
pre = DOM.mkDOM (DOM.IsDynamic true) "pre"

pre' :: Array ReactElement -> ReactElement
pre' = pre []

progress :: Array Props -> Array ReactElement -> ReactElement
progress = DOM.mkDOM (DOM.IsDynamic true) "progress"

progress' :: Array ReactElement -> ReactElement
progress' = progress []

q :: Array Props -> Array ReactElement -> ReactElement
q = DOM.mkDOM (DOM.IsDynamic true) "q"

q' :: Array ReactElement -> ReactElement
q' = q []

rp :: Array Props -> Array ReactElement -> ReactElement
rp = DOM.mkDOM (DOM.IsDynamic true) "rp"

rp' :: Array ReactElement -> ReactElement
rp' = rp []

rt :: Array Props -> Array ReactElement -> ReactElement
rt = DOM.mkDOM (DOM.IsDynamic true) "rt"

rt' :: Array ReactElement -> ReactElement
rt' = rt []

ruby :: Array Props -> Array ReactElement -> ReactElement
ruby = DOM.mkDOM (DOM.IsDynamic true) "ruby"

ruby' :: Array ReactElement -> ReactElement
ruby' = ruby []

s :: Array Props -> Array ReactElement -> ReactElement
s = DOM.mkDOM (DOM.IsDynamic true) "s"

s' :: Array ReactElement -> ReactElement
s' = s []

samp :: Array Props -> Array ReactElement -> ReactElement
samp = DOM.mkDOM (DOM.IsDynamic true) "samp"

samp' :: Array ReactElement -> ReactElement
samp' = samp []

script :: Array Props -> Array ReactElement -> ReactElement
script = DOM.mkDOM (DOM.IsDynamic true) "script"

script' :: Array ReactElement -> ReactElement
script' = script []

section :: Array Props -> Array ReactElement -> ReactElement
section = DOM.mkDOM (DOM.IsDynamic true) "section"

section' :: Array ReactElement -> ReactElement
section' = section []

select :: Array Props -> Array ReactElement -> ReactElement
select = DOM.mkDOM (DOM.IsDynamic true) "select"

select' :: Array ReactElement -> ReactElement
select' = select []

small :: Array Props -> Array ReactElement -> ReactElement
small = DOM.mkDOM (DOM.IsDynamic true) "small"

small' :: Array ReactElement -> ReactElement
small' = small []

source :: Array Props -> Array ReactElement -> ReactElement
source = DOM.mkDOM (DOM.IsDynamic true) "source"

source' :: Array ReactElement -> ReactElement
source' = source []

span :: Array Props -> Array ReactElement -> ReactElement
span = DOM.mkDOM (DOM.IsDynamic true) "span"

span' :: Array ReactElement -> ReactElement
span' = span []

strong :: Array Props -> Array ReactElement -> ReactElement
strong = DOM.mkDOM (DOM.IsDynamic true) "strong"

strong' :: Array ReactElement -> ReactElement
strong' = strong []

style :: Array Props -> Array ReactElement -> ReactElement
style = DOM.mkDOM (DOM.IsDynamic true) "style"

style' :: Array ReactElement -> ReactElement
style' = style []

sub :: Array Props -> Array ReactElement -> ReactElement
sub = DOM.mkDOM (DOM.IsDynamic true) "sub"

sub' :: Array ReactElement -> ReactElement
sub' = sub []

summary :: Array Props -> Array ReactElement -> ReactElement
summary = DOM.mkDOM (DOM.IsDynamic true) "summary"

summary' :: Array ReactElement -> ReactElement
summary' = summary []

sup :: Array Props -> Array ReactElement -> ReactElement
sup = DOM.mkDOM (DOM.IsDynamic true) "sup"

sup' :: Array ReactElement -> ReactElement
sup' = sup []

table :: Array Props -> Array ReactElement -> ReactElement
table = DOM.mkDOM (DOM.IsDynamic true) "table"

table' :: Array ReactElement -> ReactElement
table' = table []

tbody :: Array Props -> Array ReactElement -> ReactElement
tbody = DOM.mkDOM (DOM.IsDynamic true) "tbody"

tbody' :: Array ReactElement -> ReactElement
tbody' = tbody []

td :: Array Props -> Array ReactElement -> ReactElement
td = DOM.mkDOM (DOM.IsDynamic true) "td"

td' :: Array ReactElement -> ReactElement
td' = td []

textarea :: Array Props -> Array ReactElement -> ReactElement
textarea = DOM.mkDOM (DOM.IsDynamic true) "textarea"

textarea' :: Array ReactElement -> ReactElement
textarea' = textarea []

tfoot :: Array Props -> Array ReactElement -> ReactElement
tfoot = DOM.mkDOM (DOM.IsDynamic true) "tfoot"

tfoot' :: Array ReactElement -> ReactElement
tfoot' = tfoot []

th :: Array Props -> Array ReactElement -> ReactElement
th = DOM.mkDOM (DOM.IsDynamic true) "th"

th' :: Array ReactElement -> ReactElement
th' = th []

thead :: Array Props -> Array ReactElement -> ReactElement
thead = DOM.mkDOM (DOM.IsDynamic true) "thead"

thead' :: Array ReactElement -> ReactElement
thead' = thead []

time :: Array Props -> Array ReactElement -> ReactElement
time = DOM.mkDOM (DOM.IsDynamic true) "time"

time' :: Array ReactElement -> ReactElement
time' = time []

title :: Array Props -> Array ReactElement -> ReactElement
title = DOM.mkDOM (DOM.IsDynamic true) "title"

title' :: Array ReactElement -> ReactElement
title' = title []

tr :: Array Props -> Array ReactElement -> ReactElement
tr = DOM.mkDOM (DOM.IsDynamic true) "tr"

tr' :: Array ReactElement -> ReactElement
tr' = tr []

track :: Array Props -> Array ReactElement -> ReactElement
track = DOM.mkDOM (DOM.IsDynamic true) "track"

track' :: Array ReactElement -> ReactElement
track' = track []

u :: Array Props -> Array ReactElement -> ReactElement
u = DOM.mkDOM (DOM.IsDynamic true) "u"

u' :: Array ReactElement -> ReactElement
u' = u []

ul :: Array Props -> Array ReactElement -> ReactElement
ul = DOM.mkDOM (DOM.IsDynamic true) "ul"

ul' :: Array ReactElement -> ReactElement
ul' = ul []

var :: Array Props -> Array ReactElement -> ReactElement
var = DOM.mkDOM (DOM.IsDynamic true) "var"

var' :: Array ReactElement -> ReactElement
var' = var []

video :: Array Props -> Array ReactElement -> ReactElement
video = DOM.mkDOM (DOM.IsDynamic true) "video"

video' :: Array ReactElement -> ReactElement
video' = video []

wbr :: Array Props -> Array ReactElement -> ReactElement
wbr = DOM.mkDOM (DOM.IsDynamic true) "body"

wbr' :: Array ReactElement -> ReactElement
wbr' = wbr []
