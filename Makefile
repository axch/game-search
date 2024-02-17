DUMP_FLAGS = --ghc-options="-ddump-simpl" \
	     --ghc-options="-ddump-stg-final" \
	     --ghc-options="-dsuppress-all" \
	     --ghc-options="-dno-suppress-type-signatures" \
	     --ghc-options="-ddump-to-file"

tic-tac-toe-heap.pdf:
	stack build --work-dir .stack-work-prof --trace \
	  && time stack --work-dir .stack-work-prof exec \
	    -- tic-tac-toe 20 500 500 +RTS -hd && hp2ps tic-tac-toe.hp \
	  && ps2pdf tic-tac-toe.ps

.PHONY: tic-tac-toe.prof
tic-tac-toe.prof:
	stack build --work-dir .stack-work-prof --trace --ghc-options="-fno-prof-auto" $(DUMP_FLAGS) \
	  && time stack exec --work-dir .stack-work-prof --trace \
	    -- tic-tac-toe 20 500 500 +RTS -p

time:
	stack build && time stack exec tic-tac-toe 20 500 500
