mcts-exe.pdf:
	stack build --profile && time stack exec -- mcts-exe 100 200 +RTS -hd && hp2ps mcts-exe.hp && ps2pdf mcts-exe.ps

mcts-exe.prof:
	stack build --profile && time stack exec -- mcts-exe 100 200 +RTS -p
