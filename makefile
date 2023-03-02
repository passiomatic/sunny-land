all: run

run:
	elm reactor

build:
	elm make src/Main.elm  --output=build/elm.js

clean: 
	rm -rf build/