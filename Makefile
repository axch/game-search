game-search-exe.pdf:
	stack build --profile && time stack exec -- game-search-exe 100 200 +RTS -hd && hp2ps game-search-exe.hp && ps2pdf game-search-exe.ps

game-search-exe.prof:
	stack build --profile && time stack exec -- game-search-exe 100 200 +RTS -p

time:
	stack build && time stack exec game-search-exe 100 200
