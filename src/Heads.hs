{-# LANGUAGE MultiParamTypeClasses #-}

module Heads where

import Types

-- The Heads game: flip a fair coin until you get heads.

-- This is a test of the game history tracking feature of the exact
-- solution engine.  The idea is to track the number of times one
-- didn't win before one won.

data Heads = Play | Won deriving (Eq, Show)

instance Renderable Heads where
    render = putStrLn . show

instance RGame Heads () where
    moves Play = [()]
    moves Won = []
    r_move _ _ = Probabilities [(1/2, Play), (1/2, Won)]
    valid _ Play = True
    valid _ Won = False
    start = Play
    finished Play = False
    finished Won = True
    payoff Play (Player 0) = Nothing
    payoff Won (Player 0) = Just 1
    current _ = Player 0
