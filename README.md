A Suite of Simple Generic Game Players
--------------------------------------

This package contains a Haskell implementation of a (currently small)
suite of generic game players.

The primary implemented play strategy is UCT, parameterized by the
number of playouts to do before choosing a move to play.  We also have
- Uniformly random
- UCB1
- "Take Obvious Moves" modification (see below)

The players are generic in the sense that they can play any game that
implements the Game interface in `src/Types.hs`.  This is not to be
confused with "General Game Playing", which has come to mean being
able to play a new game reasonably, specifically ones whose rules,
written in a specific Datalog variant, are given to you at the start
of the game.

Code organization:
- Players themselves are in `src/MCTS.hs`
- Game interface is in `src/Types.hs`
- An implementation of generalized tic-tac-toe to that interface in
  `src/TicTacToe.hs`
- Game runner in `src/Umpire.hs`
- Overall driver for running tournaments or playing vs human in
  `app/Main.hs`

Status: The player has been tested on m,n,k games (a generalization of
tic-tac-toe), and seems to behave generally as expected.  I get the
impression that it's pretty weak per unit computation, but seems to
work.  No evidence of being broken on deterministic games, at least.

Note: Current handling of stochastic games is grossly wrong---will
conflate different random results of the same move.

Explanation: The "Take Obvious Moves" modification comes from the idea
that in tic-tac-toe (and similar games), it can be more efficient to
compute one-move wins (and, conversely, moves that must be taken to
prevent a one-move win by the opponent) using game-specific knowledge
than searching each possible move and checking for a win.  The playout
strategy can be adjusted to play such moves, which increases overall
playing strength (at least somewhat).
