{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Expectimax where

import Data.Function (on)
import Data.IORef
import Data.List (maximumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.IO.Unsafe

import Types

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
