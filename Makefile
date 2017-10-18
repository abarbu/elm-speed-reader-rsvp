all: build

build: src/Main.elm
	elm-make src/Main.elm --output build/main.js

watch:
	watch make src

start: build
	cd build && browser-sync start --server --files . --reload-delay 300

setup:
	elm-github-install
	bower install

install:
	npm install -g elm
	npm install -g watch
	npm install -g elm-github-install
	npm install -g browser-sync
	npm install -g bower
