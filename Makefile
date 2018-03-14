all: runtest lib

lib:
	jbuilder build --dev @install

runtest: lib
	jbuilder runtest --dev --force
