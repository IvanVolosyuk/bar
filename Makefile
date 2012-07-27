all: dzen

dzen: dzen.hs Xpm.hs
	ghc --make dzen
