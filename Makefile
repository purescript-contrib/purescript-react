.DELETE_ON_ERROR:
PSC_OPTS =

all: lib example

lib:: lib/React.js
example:: example/app.js example/bower_components

example/bower_components:
	(cd example && bower install react)

clean:
	rm -f lib/React.js example/app.js
	rm -rf example/bower_components/

lib/React.js: src/React.purs src/React/DOM.purs
	mkdir -p $(@D)
	psc $(PSC_OPTS) $^ \
		--output $@ \
		--module React --module React.DOM

example/app.js: src/React.purs src/React/DOM.purs example/app.purs
	psc $(PSC_OPTS) $^ \
		--output $@ \
		--main --module Main
