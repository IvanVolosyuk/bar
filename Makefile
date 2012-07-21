all: geticon dzen

dzen: dzen.hs
	ghc --make dzen

geticon: geticon.cc
	g++ -o geticon geticon.cc -lX11 -lpng
