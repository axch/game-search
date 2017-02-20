{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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
    type Player Heads = Solitaire
    moves Play = [()]
    moves Won = []
    r_move _ _ = Probabilities [(1/2, Play), (1/2, Won)]
    valid _ Play = True
    valid _ Won = False
    start = Play
    finished Play = False
    finished Won = True
    payoff Play Self = Nothing
    payoff Won Self = Just 1
    current _ = Self

-- Here is a variant that gives a specific budget of turns to win, or
-- you lose.

data TimedHeads = TPlay Int | TWon deriving (Eq, Show)

instance Renderable TimedHeads where
    render = putStrLn . show

instance RGame TimedHeads () where
    type Player TimedHeads = Solitaire
    moves (TPlay _) = [()]
    moves TWon = []
    r_move _ (TPlay n) = Probabilities [(1/2, (TPlay $ n-1)), (1/2, TWon)]
    valid _ (TPlay _) = True
    valid _ TWon = False
    start = TPlay 5
    finished (TPlay 0) = True
    finished (TPlay _) = False
    finished TWon = True
    payoff (TPlay 0) Self = Just 0
    payoff (TPlay _) Self = Nothing
    payoff TWon Self = Just 1
    current _ = Self
