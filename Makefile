all: runtest lib

lib:
	dune build @install

runtest: lib
	dune runtest --force
