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

class (Eq a, Renderable a, Move m) => Game a m | a -> m where
    moves :: a -> [m]
    move  :: m -> a -> a
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
