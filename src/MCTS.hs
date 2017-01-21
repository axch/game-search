module MCTS where

import Data.Random (RVar)
import Data.Random.Distribution (rvar)
import qualified Data.Random.Distribution.Categorical as Cat

import Types

ones :: [Double]
ones = 1:ones

play_out :: (Game a m) => a -> RVar a -- Where the returned state is terminal
play_out g | finished g = return g
           | otherwise = do
  m <- rvar $ Cat.normalizeCategoricalPs $ Cat.fromList $ zip ones $ moves g
  g' <- move m g
  return g'
