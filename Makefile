CC = /usr/lib/elm/0.12.3/elm
build = build/
runtime = elm/elm-runtime.js
src = src
flags = --make --set-runtime=$(runtime) --build-dir=$(build) --src-dir=$(src)

all: compile

compile: build/Tetris.js build/elm/

build/elm/: elm/
	cp elm/ build/ -r

build/Tetris.js: src/ snd/
	$(CC) $(flags) -o Tetris.elm
	cp src/Tetris.html build/Tetris.html
	cp src/TetrisMusic.js build/TetrisMusic.js
	cp snd/ build/snd -r

clean: build
	rm build -rf
	rm cache -rf

