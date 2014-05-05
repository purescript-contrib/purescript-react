.DELETE_ON_ERROR:
PSC_OPTS =

all: lib example

lib:: lib/React.js
example:: example/app.js

clean:
	rm -f lib/React.js example/app.js

lib/React.js: src/React.purs
	mkdir -p $(@D)
	psc $(PSC_OPTS) $< \
		--output $@ \
		--module React

example/app.js: src/React.purs example/app.purs
	psc $(PSC_OPTS) $^ \
		--output $@ \
		--main --module Main
