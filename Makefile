all: benchmark-table.md

benchmark: benchmark.hs MonadList.hs UnsafeSetField.hs Holes.hs UnsafeSetFieldCmm.cmm
	ghc --make -O2 $^

benchmark-results.csv: benchmark
	rm -f benchmark-results.csv
	./benchmark -L 10 --csv $@

benchmark-table.md: benchmark-results.csv
	runhaskell transpose.hs < $^ > $@
