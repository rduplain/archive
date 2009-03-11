all: download

run: download
	./download

download: src/Main.lhs src/Browse.lhs src/Project.lhs src/CAS.lhs
	ghc --make -o $@ -isrc $^
