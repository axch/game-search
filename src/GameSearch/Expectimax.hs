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

import Data.Function (on)
import Data.IORef
import Data.List (maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.IO.Unsafe

import GameSearch.Types

-- Compute the move (if not terminal) that maximizes the expected
-- utility from this position, as well as the value thereof, assuming
-- solitaire and exhaustive search.

best_move :: forall a m. (Ord a, RGame a m, Player a ~ Solitaire) => a -> (Maybe m, Double)
best_move = answer where
  go g | finished g = (Nothing, fromJust $ payoff g Self)
       | otherwise = maximumBy (compare `on` snd) $ map evaluate $ moves g
    where
      evaluate m = (Just m, expectation $ fmap (snd . answer) results)
          where
            results :: Probabilities Double a
            results = r_move m g
  answer = memoize go

memoize :: (Ord a) => (a -> r) -> a -> r
memoize f = unsafePerformIO (do
  cacheRef <- newIORef M.empty
  return $ \x -> unsafePerformIO (do
                   cache <- readIORef cacheRef
                   case M.lookup x cache of
                     (Just v) -> return v
                     Nothing -> do
                       let v = f x
                       modifyIORef' cacheRef (M.insert x v)
                       return v))
