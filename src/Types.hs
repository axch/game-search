{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Data.Maybe (isJust)

import Data.Random (MonadRandom, sample)

newtype Player = Player Int deriving (Eq, Ord, Show)

class Move a where

class Renderable a where
    render :: a -> IO ()

class CtxParseable c a where
    ctx_parse :: c -> String -> Either String a

-- A list tagged with Probabilities is expected to have the fst
-- components sum to 1.
newtype Probabilities p a = Probabilities [(p, a)]

-- Nomenclature:
-- - Game is the deterministic case
-- - RGame and r_move is a random game (but with known outcome probabilities)
-- - [Later] SGame and s_move is for a sampling-only random game
--   (where the outcome probabilities are not exposed to analysis,
--   e.g. for performance).

class (Eq a, Renderable a, Move m) => RGame a m | a -> m where
    moves :: a -> [m]
    r_move :: (Fractional p) => m -> a -> Probabilities p a
    valid :: m -> a -> Bool
    start :: a
    finished :: a -> Bool
    finished a = isJust $ payoff a (Player 0)
    -- All the proofs of UCT properties I have found expect rewards to be in [0,1]
    payoff :: a -> Player -> Maybe Double -- Nothing if the game isn't over yet
    current :: a -> Player
    known_one_move_wins :: a -> [m]
    known_one_move_wins = const []
    known_one_move_blocks :: a -> [m]
    known_one_move_blocks = const []

-- Decision: A Move is meant to be applicable to many positions (such
-- as placing a piece in Go), as this seems more common than moves
-- being bound to the positions they come from.

class (RGame a m) => Game a m where
    move :: m -> a -> a

default_r_move :: (Game a m, Num p) => m -> a -> Probabilities p a
default_r_move m g = Probabilities [(fromIntegral 1, move m g)]
