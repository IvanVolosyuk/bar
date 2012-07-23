all: geticon dzen lower

dzen: dzen.hs Xpm.hs
	ghc --make dzen

geticon: geticon.cc
	g++ -o geticon geticon.cc -lX11 -lpng

lower: lower.cc
	g++ -o lower lower.cc -lX11
