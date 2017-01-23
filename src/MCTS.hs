module MCTS where

import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map as M

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

-- Map each move to the total payoff obtained by going there, together
-- with the number of times we've gone there
data OneLevel m = OneLevel Int (M.Map m (Double, Int))

update_once :: (Ord m, Game a m) => a -> m -> OneLevel m -> RVar (OneLevel m)
update_once g m (OneLevel tot state) = do
  g' <- move m g
  g'' <- play_out g'
  let (Just p) = payoff g'' (current g)
  return $ OneLevel (tot+1) $ M.adjust (\(old_p, n) -> (old_p + p, n + 1)) m state

exploration_parameter :: Double
exploration_parameter = sqrt 2

select_move :: OneLevel m -> RVar m
select_move (OneLevel tot state) = return m where
    value (_, (_, 0)) = 1/0
    value (_, (score, tries)) =
        score / fromIntegral tries
        + exploration_parameter * sqrt (log (fromIntegral tot) / fromIntegral tries)
    (m, (_, _)) = maximumBy (compare `on` value) $ M.toList state
