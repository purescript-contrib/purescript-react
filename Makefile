.DELETE_ON_ERROR:
PSC_OPTS =

all: lib example

lib:: lib/React.js
examples:: example/app example/tutorial

example/app:: example/app/app.js example/app/bower_components
example/tutorial:: example/tutorial/tutorial.js example/tutorial/bower_components

example/app/bower_components:
	(cd $(@D) && bower install react)

example/tutorial/bower_components:
	(cd $(@D) && bower install)

clean:
	rm -f lib/React.js example/app/app.js example/tutorial/tutorial.js

lib/React.js: src/React.purs src/React/DOM.purs
	mkdir -p $(@D)
	psc $(PSC_OPTS) $^ \
		--output $@ \
		--module React --module React.DOM

example/app/app.js: src/React.purs src/React/DOM.purs example/app/app.purs
	psc $(PSC_OPTS) $^ \
		--output $@ \
		--main --module Main

example/tutorial/tutorial.js: src/React.purs src/React/DOM.purs example/tutorial/tutorial.purs
	psc $(PSC_OPTS) $^ $(shell find $(@D)/bower_components -name '*.purs') \
		--output $@ \
		--module Tutorial --main Tutorial
