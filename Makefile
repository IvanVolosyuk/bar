all: dzen bar

clean:
	rm *.o *.hi bar dzen

dzen: dzen.hs Xpm.hs Utils.hs
	ghc --make dzen

bar: bar.hs Top.hs Utils.hs Icon.hs DzenParse.hs
	ghc --make -threaded bar -O2 -funfolding-use-threshold=16 -optc-O3 -fexcess-precision 

# Need parsec library
# libghc6-parsec3-dev
