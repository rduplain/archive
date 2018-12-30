all: test

install:
	npm install

test: install
	./node_modules/.bin/shadow-cljs compile test
