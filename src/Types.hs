{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Data.Maybe (isJust)

import Data.Random (RVar, sample)

type Ran = RVar

newtype Player = Player Int deriving (Eq, Ord, Show)

class Move a where

class Renderable a where
    render :: a -> IO ()

class (Eq a, Renderable a, Move m) => Game a m | a -> m where
    moves :: a -> [m]
    move  :: m -> a -> (Ran a) -- Random if it's Nature's move; TODO: make the distribution enumerable
    valid :: m -> a -> Bool
    start :: a
    finished :: a -> Bool
    finished a = isJust $ payoff a (Player 0)
    -- All the proofs I have found expect rewards to be in [0,1]
    payoff :: a -> Player -> Maybe Double -- Nothing if the game isn't over yet
    current :: a -> Player

-- Decision: A Move is meant to be applicable to many positions (such
-- as placing a piece in Go), as this seems more common than moves
-- being bound to the positions they come from.

sampleIO :: Ran a -> IO a
sampleIO = sample
