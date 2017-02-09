{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Control.Monad (ap)
import Data.Maybe (isJust)

import Data.Random (MonadRandom, sample)

newtype Player = Player Int deriving (Eq, Ord, Show)

class Renderable a where
    render :: a -> IO ()

class CtxParseable c a where
    ctx_parse :: c -> String -> Either String a

-- A list tagged with Probabilities is expected to have the fst
-- components sum to 1.
newtype Probabilities p a = Probabilities [(p, a)]

-- Objects a that support a "mixture" operation using probabilities of
-- type p.
class Mixable a p where
    expectation :: Probabilities p a -> a

instance Mixable Double Double where
    expectation (Probabilities items) = sum $ map (\(p, a) -> p * a) items

-- Caveat: This instance does not try to collapse equal a objects, to
-- avoid needing more instances.
instance (Num p) => Mixable (Probabilities p a) p where
    expectation (Probabilities items) = Probabilities $ concatMap scale items
        where
          scale (p, (Probabilities subitems)) = map (\(p1, a) -> (p * p1, a)) subitems

instance Functor (Probabilities p) where
    fmap f (Probabilities items) = Probabilities $ zip ps as
        where
          ps = map fst items
          as = map (f . snd) items

instance (Num p) => Applicative (Probabilities p) where
    pure = return
    (<*>) = ap

instance (Num p) => Monad (Probabilities p) where
    return a = Probabilities [(1, a)]
    (>>=) ps f = expectation $ fmap f ps

-- Nomenclature:
-- - Game is the deterministic case
-- - m is the move type
-- - RGame and r_move is a random game (but with known outcome probabilities)
-- - [Later] SGame and s_move is for a sampling-only random game
--   (where the outcome probabilities are not exposed to analysis,
--   e.g. for performance).

class (Eq a, Renderable a) => RGame a m | a -> m where
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

-- Decision: A move is meant to be applicable to many positions (such
-- as placing a piece in Go), as this seems more common than moves
-- being bound to the positions they come from.

class (RGame a m) => Game a m where
    move :: m -> a -> a

default_r_move :: (Game a m, Num p) => m -> a -> Probabilities p a
default_r_move m g = Probabilities [(fromIntegral 1, move m g)]
