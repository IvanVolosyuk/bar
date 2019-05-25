all: dzen bar bar2

clean:
	rm *.o *.hi bar

bar: bar.hs Top.hs Utils.hs Icon.hs DzenParse.hs
	ghc --make -threaded bar -O2 -funfolding-use-threshold=16 -optc-O3 -fexcess-precision 

bar2: bar2.hs Top.hs Utils.hs Icon.hs DzenParse.hs Timer.hs
	ghc --make -threaded bar2 -O2 -funfolding-use-threshold=16 -optc-O3 -fexcess-precision 
