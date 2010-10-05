all:
	ghc --make -o Bench -i.. -DMETHOD=$(METHOD) Bench.hs

clean:
	(cd ..; find . -name "*.hi" -print -exec rm '{}' \;)
	(cd ..; find . -name "*.o" -print -exec rm '{}' \;)
	rm -f Bench

distclean:
	make clean
	rm -f *.csv *.png
