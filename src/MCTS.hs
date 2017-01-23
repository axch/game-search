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

uniform_choose :: (Game a m) => a -> RVar m
uniform_choose g = rvar $ Cat.normalizeCategoricalPs $ Cat.fromList $ zip ones $ moves g

play_out :: (Game a m) => a -> RVar a -- Where the returned state is terminal
play_out g | finished g = return g
           | otherwise = do m <- uniform_choose g
                            g' <- move m g
                            play_out g'

-- Map each move to the total payoff obtained by going there, together
-- with the number of times we've gone there
data OneLevel m = OneLevel Int (M.Map m (Double, Int))

empty_level :: (Ord m) => [m] -> OneLevel m
empty_level ms = OneLevel 0 $ M.fromList $ zip ms $ repeat (0, 0)

update_once :: (Ord m, Game a m) => a -> m -> OneLevel m -> RVar (OneLevel m)
update_once g m (OneLevel tot state) = do
  g' <- move m g
  g'' <- play_out g'
  let (Just p) = payoff g'' (current g)
  return $ OneLevel (tot+1) $ M.adjust (\(old_p, n) -> (old_p + p, n + 1)) m state

exploration_parameter :: Double
exploration_parameter = sqrt 2

-- Choose a move to explore
-- TODO: they say I ought to break ties randomly, but for now just
-- taking the lexicographically earliest move.
select_move :: OneLevel m -> RVar m
select_move (OneLevel tot state) = return m where
    value (_, (_, 0)) = 1/0
    value (_, (score, tries)) =
        score / fromIntegral tries
        + exploration_parameter * sqrt (log (fromIntegral tot) / fromIntegral tries)
    (m, (_, _)) = maximumBy (compare `on` value) $ M.toList state

-- Choose a move to return once exploration is done: most explored, in
-- this case
select_final_move :: OneLevel m -> RVar m
select_final_move (OneLevel _ state) = return m where
    value (_, (_, tries)) = tries
    (m, (_, _)) = maximumBy (compare `on` value) $ M.toList state

ucb1_choose :: (Ord m, Game a m) => Int -> a -> RVar m
ucb1_choose tries g = go tries $ empty_level $ moves g where
    go tries level | tries == 0 = select_final_move level
                   | otherwise = do
      m <- select_move level
      level' <- update_once g m level
      go (tries - 1) level'
