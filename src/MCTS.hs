module MCTS where

import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Base (assert)

import Data.Random
import qualified Data.Random.Distribution.Uniform as Uni

import Types

ones :: [Double]
ones = 1:ones

uniform_choose :: (MonadRandom r) => (Game a m) => a -> r m
uniform_choose g = do
  index <- sample $ Uni.uniform 0 (n-1)
  return $ ms!!index
    where
      ms = moves g
      n = length ms

play_out :: (Game a m, MonadRandom r) => (a -> r m) -> a -> r a -- Where the returned state is terminal
play_out strat = go where
  go g | finished g = return g
       | otherwise = do m <- strat g
                        g' <- move m g
                        go g'

----------------------------------------------------------------------
-- UCB 1

-- Map each move to the total payoff obtained by going there, together
-- with the number of times we've gone there
data OneLevel m = OneLevel Int (M.Map m (Double, Int))

empty_level :: (Ord m) => [m] -> OneLevel m
empty_level ms = OneLevel 0 $ M.fromList $ zip ms $ repeat (0, 0)

update_once :: (Ord m, Game a m, MonadRandom r) => a -> m -> OneLevel m -> r (OneLevel m)
update_once g m (OneLevel tot state) = do
  g' <- move m g
  g'' <- play_out uniform_choose g'
  let (Just p) = payoff g'' (current g)
  return $ OneLevel (tot+1) $ M.adjust (\(old_p, n) -> (old_p + p, n + 1)) m state

exploration_parameter :: Double
exploration_parameter = sqrt 2

-- Choose a move to explore
-- TODO: they say I ought to break ties randomly, but for now just
-- taking the lexicographically earliest move.
select_move :: (MonadRandom r) => OneLevel m -> r m
select_move (OneLevel tot state) = return m where
    value (_, (_, 0)) = 1/0
    value (_, (score, tries)) =
        score / fromIntegral tries
        + exploration_parameter * sqrt (log (fromIntegral tot) / fromIntegral tries)
    (m, (_, _)) = maximumBy (compare `on` value) $ M.toList state

-- Choose a move to return once exploration is done: most explored, in
-- this case
select_final_move :: (MonadRandom r) => OneLevel m -> r m
select_final_move (OneLevel _ state) = return m where
    value (_, (_, tries)) = tries
    (m, (_, _)) = maximumBy (compare `on` value) $ M.toList state

ucb1_choose :: (Ord m, Game a m, MonadRandom r) => Int -> a -> r m
ucb1_choose tries g = go tries $ empty_level $ moves g where
    go tries level | tries == 0 = select_final_move level
                   | otherwise = do
      m <- select_move level
      level' <- update_once g m level
      go (tries - 1) level'

----------------------------------------------------------------------
-- UCT

-- The standard UCT tree policy makes an interesting choice, namely to
-- grow the tree by one node for each play-out.

-- Invariants:
-- - The first int is the number of play-outs performed from this node
-- - The map is always complete, i.e. all the moves from the state
--   this node is for are present
-- - The subtree at a key is Nothing if that node has not been expanded
-- - The latter can happen with a non-zero trial count if that game
--   is finished.
data UCTree m = UCTree Int (M.Map m (Maybe (UCTree m), Double, Int))

empty_subtree :: (Ord m) => [m] -> UCTree m
empty_subtree ms = assert (length ms > 0) $ UCTree 0 $ M.fromList $ zip ms $ repeat (Nothing, 0, 0)

-- Choose a move to explore
-- TODO: they say I ought to break ties randomly, but for now just
-- taking the lexicographically earliest move.
select_move' :: (MonadRandom r) => UCTree m -> r m
select_move' (UCTree tot state) = return m where
    value (_, (_, _, 0)) = 1/0
    value (_, (_, score, tries)) =
        score / fromIntegral tries
        + exploration_parameter * sqrt (log (fromIntegral tot) / fromIntegral tries)
    m = fst $ maximumBy (compare `on` value) $ M.toList state

-- Choose a game state to evaluate with the given (random) evaluation
-- function, evaluate it, update the tree, and return the evaluation
-- function for the caller's benefit.
-- TODO: Since the move's result may be stochastic, I have to know what
-- Nature chose in order to select the proper sub-tree.
at_selected_state :: (Game a m, Ord m, MonadRandom r) => (a -> r (Player -> Double)) -> a -> UCTree m
                     -> r ((UCTree m), (Player -> Double))
at_selected_state eval g t@(UCTree tot state) = do
  m <- select_move' t
  g' <- move m g
  case fromJust $ M.lookup m state of
    (Just subtree, reward, tries) ->
        do (subtree', win) <- at_selected_state eval g' subtree
           let state' = M.insert m (Just subtree', reward + win (current g), tries + 1) state
           return (UCTree (tot+1) state', win)
    (Nothing, reward, tries) ->
        do win <- eval g'
           let subtree' = if finished g' then Nothing
                          else Just $ empty_subtree (moves g')
               state' = M.insert m (subtree', reward + win (current g), tries + 1) state
           return (UCTree (tot+1) state', win)

one_play_out :: (Game a m, MonadRandom r) => a -> r (Player -> Double)
one_play_out g = do
  end <- play_out uniform_choose g
  return $ fromJust . payoff end

-- Choose a move to return once exploration is done: most explored, in
-- this case
select_final_move' :: (MonadRandom r) => UCTree m -> r m
select_final_move' (UCTree _ state) = return m where
    value (_, (_, _, tries)) = tries
    (m, _) = maximumBy (compare `on` value) $ M.toList state

uct_choose :: (Game a m, Ord m, MonadRandom r) => Int -> a -> r m
uct_choose tries g = go tries $ empty_subtree $ moves g where
    go tries tree | tries == 0 = select_final_move' tree
                  | otherwise = do
      (tree', _) <- at_selected_state one_play_out g tree
      go (tries-1) tree'
