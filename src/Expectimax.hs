{-# LANGUAGE ScopedTypeVariables #-}

module Expectimax where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromJust)

import Types

-- Compute the move (if not terminal) that maximizes the expected
-- utility from this position, as well as the value thereof, assuming
-- solitaire and exhaustive search.

best_move :: forall a m. (RGame a m) => a -> (Maybe m, Double)
best_move g | finished g = (Nothing, fromJust $ payoff g $ Player 0)
            | otherwise = maximumBy (compare `on` snd) $ map evaluate $ moves g
    where
      evaluate m = (Just m, expectation $ fmap (snd . best_move) results)
          where
            results :: Probabilities Double a
            results = r_move m g

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
