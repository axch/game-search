module MCTS where

import Data.Random (RVar)
import Data.Random.Distribution (rvar)
import qualified Data.Random.Distribution.Categorical as Cat

import Types

ones :: [Double]
ones = 1:ones

uniform_step :: (Game a m) => a -> RVar a
uniform_step g = do
  m <- rvar $ Cat.normalizeCategoricalPs $ Cat.fromList $ zip ones $ moves g
  g' <- move m g
  return g'

play_out :: (Game a m) => a -> RVar a -- Where the returned state is terminal
play_out g | finished g = return g
           | otherwise = do g' <- uniform_step g
                            play_out g'
