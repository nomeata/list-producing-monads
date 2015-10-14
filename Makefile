all: README.md graphs.svg

benchmark: benchmark.hs MonadList.hs UnsafeSetField.hs Holes.hs UnsafeSetFieldCmm.cmm
	ghc --make -O2 $^

benchmark-results.csv: benchmark
	rm -f benchmark-results.csv
	./benchmark -L 10 --csv $@

README.md: benchmark-results.csv transpose.hs
	runhaskell transpose.hs < $< > $@

graphs.svg: benchmark-results.csv plot.hs
	runhaskell plot.hs < $<
