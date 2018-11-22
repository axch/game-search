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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GameSearch.Expectimax where

-- Exact Expectimax computations (for games whose state space is small
-- enough for this to be feasible).

import Control.Monad.Trans.Writer.Strict
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Traversable (mapM)

import GameSearch.Types

-- Compute the move (if not terminal) that maximizes the expected
-- utility from this position, as well as the value thereof, assuming
-- solitaire and exhaustive search.

type Caching a r = State (M.Map a r)

best_move :: forall a m. (Ord a, RGame a m, Player a ~ Solitaire) => a -> (Maybe m, Double)
best_move g_start = evalState (expectimax g_start) M.empty

best_moves :: forall a m. (Ord a, RGame a m, Player a ~ Solitaire) => a -> M.Map a (Maybe m, Double)
best_moves g_start = execState (expectimax g_start) M.empty

expectimax :: forall a m. (Ord a, RGame a m, Player a ~ Solitaire)
              => a -> Caching a (Maybe m, Double) (Maybe m, Double)
expectimax = answer where
  go g | finished g = return (Nothing, fromJust $ payoff g Self)
       | otherwise = liftM (maximumBy (compare `on` snd)) $ mapM evaluate $ moves g
    where
      evaluate :: m -> Caching a (Maybe m, Double) (Maybe m, Double)
      evaluate m = do
        let results :: Probabilities Double a
            results = r_move m g
        subanswers <- mapM (liftM snd . answer) results
        return (Just m, expectation subanswers)

  answer :: a -> Caching a (Maybe m, Double) (Maybe m, Double)
  answer = memoize go

memoize :: (Ord a) => (a -> Caching a r r) -> a -> Caching a r r
memoize f x = do
  cache <- get
  case M.lookup x cache of
    (Just v) -> return v
    Nothing -> do
                v <- f x
                modify (M.insert x v)
                return v

visit_probabilities :: forall a m p. (Ord a, RGame a m, Player a ~ Solitaire, Fractional p) =>
                       a -> Caching a (Maybe m, Double) (M.Map a p)
visit_probabilities start = execWriterT $ go $ M.singleton start 1 where
  go queue | M.null queue = return ()
           | otherwise = step queue >>= go
  step :: M.Map a p -> WriterT (M.Map a p) (Caching a (Maybe m, Double)) (M.Map a p)
  step queue = do
    let ((game, prob), queue') = M.deleteFindMax queue
    tell $ M.singleton game prob
    (maybe_move, _) <- lift $ expectimax game
    case maybe_move of
      (Just m) -> return $ foldl' insert queue' items where
                    (Probabilities items) = r_move m game
                    insert q (p, new_game) = M.insertWith (+) new_game (prob * p) q
      Nothing -> return queue'
