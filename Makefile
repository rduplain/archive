all: download

run: download
	./download

download: src/Main.lhs
	ghc --make -o $@ $^
