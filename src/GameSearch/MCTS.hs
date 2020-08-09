-- Copyright 2017 Alexey Radul

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module GameSearch.MCTS where

-- Monte Carlo Tree Search style players: random playouts, UCB1, UCT.

import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Base (assert)

import Data.Random
import qualified Data.Random.Distribution.Uniform as Uni

import GameSearch.Types

ones :: [Double]
ones = 1:ones

-- `uniform_choose` is a strategy that just picks a random legal move
-- uniformly.
uniform_choose :: (MonadRandom r, RGame a m) => a -> r m
uniform_choose g = do
  index <- sample $ Uni.uniform 0 (n-1)
  return $ ms!!index
    where
      ms = moves g
      n = length ms
{-# SPECIALIZE uniform_choose :: (RGame a m) => a -> IO m #-}

-- `take_obvious_plays` is a strategy that tries to take or block any
-- available game-specific one-move wins, and otherwise just picks a
-- random legal move uniformly.
take_obvious_plays :: (MonadRandom r, RGame a m) => a -> r m
take_obvious_plays g =
  case known_one_move_wins g of
    (m:_) -> return m
    [] -> case known_one_move_blocks g of
            (m:_) -> return m
            [] -> uniform_choose g
{-# SPECIALIZE take_obvious_plays :: (RGame a m) => a -> IO m #-}

-- `play_out` runs a playout, making moves until the end of the game.
play_out :: (Game a m, MonadRandom r) => (a -> r m) -> a -> r a -- Where the returned state is terminal
play_out strat = go where
  go g | finished g = return g
       | otherwise = do m <- strat g
                        go $ move m g
{-# SPECIALIZE play_out :: (Game a m) => (a -> IO m) -> a -> IO a #-}

----------------------------------------------------------------------
-- UCB 1

-- Map each move to the total payoff obtained by going there, together
-- with the number of times we've gone there
data OneLevel m = OneLevel Int (M.Map m (Double, Int))

empty_level :: (Ord m) => [m] -> OneLevel m
empty_level ms = OneLevel 0 $ M.fromList $ zip ms $ repeat (0, 0)

update_once :: (Ord m, Game a m, MonadRandom r) =>
               (a -> r m) -> a -> m -> OneLevel m -> r (OneLevel m)
update_once substrat g m (OneLevel tot state) = do
  g'' <- play_out substrat $ move m g
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

-- `ucb1_choose n substrat` is a UCB1 strategy that does n playouts
-- using `substrat`.
ucb1_choose :: (Ord m, Game a m, MonadRandom r) => Int -> (a -> r m) -> a -> r m
ucb1_choose tries substrat g = go tries $ empty_level $ moves g where
    go tries_left level | tries_left == 0 = select_final_move level
                        | otherwise = do
      m <- select_move level
      level' <- update_once substrat g m level
      go (tries_left - 1) level'

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
{-# SPECIALIZE select_move' :: UCTree m -> IO m #-}

-- Choose a game state to evaluate with the given (random) evaluation
-- function, evaluate it, update the tree, and return the evaluation
-- function for the caller's benefit.
at_selected_state :: (Game a m, Ord m, MonadRandom r) => (a -> r (Player a -> Double)) -> a -> UCTree m
                     -> r ((UCTree m), (Player a -> Double))
at_selected_state eval g t@(UCTree tot state) = do
  m <- select_move' t
  let g' = move m g
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
{-# SPECIALIZE at_selected_state :: (Game a m, Ord m) => (a -> IO (Player a -> Double)) -> a -> UCTree m
  -> IO ((UCTree m), (Player a -> Double)) #-}

one_play_out :: (Game a m, MonadRandom r) => (a -> r m) -> a -> r (Player a -> Double)
one_play_out strat g = do
  end <- play_out strat g
  return $ fromJust . payoff end
{-# SPECIALIZE one_play_out :: (Game a m) => (a -> IO m) -> a -> IO (Player a -> Double) #-}

-- Choose a move to return once exploration is done: most explored, in
-- this case
select_final_move' :: (MonadRandom r) => UCTree m -> r m
select_final_move' (UCTree _ state) = return m where
    value (_, (_, _, tries)) = tries
    (m, _) = maximumBy (compare `on` value) $ M.toList state
{-# SPECIALIZE select_final_move' :: UCTree m -> IO m #-}

-- `uct_choose n substrat` is a UCT strategy that does n playouts
-- using `substrat`.
uct_choose :: (Game a m, Ord m, MonadRandom r) => Int -> (a -> r m) -> a -> r m
uct_choose tries substrat g = go tries $ empty_subtree $ moves g where
    go tries_left tree | tries_left == 0 = select_final_move' tree
                       | otherwise = do
      (tree', _) <- at_selected_state (one_play_out substrat) g tree
      go (tries_left-1) tree'
{-# SPECIALIZE uct_choose :: (Game a m, Ord m) => Int -> (a -> IO m) -> a -> IO m #-}
