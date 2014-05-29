CC = /usr/lib/elm/0.12.3/elm
build = build/
runtime = elm/elm-runtime.js
src = src
flags = --make --set-runtime=$(runtime) --build-dir=$(build) --src-dir=$(src)

all: compile

compile: build/Tetris.html build/elm/

build/elm/: elm/
	cp elm/ build/ -r

build/Tetris.html: src/ snd/
	$(CC) $(flags) Tetris.elm
	cp snd/ build/snd -r

clean: build
	rm build -rf
	rm cache -rf

