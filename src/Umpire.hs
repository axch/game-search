module Umpire where

import Control.Monad
import GHC.Base (assert)

import Data.Random (RVar)

import Types

game :: (Game a m) => (a -> RVar m) -> a -> RVar a -- Where the returned state is terminal
game strat = go where
  go g | finished g = return g
       | otherwise = do m <- strat g
                        g' <- assert (valid m g) $ move m g
                        go g'

match :: (Monoid res, Game a m) => Int -> (a -> RVar m) -> (a -> res) -> a -> RVar res
match n strat eval start =
    liftM mconcat $ liftM (map eval) $ replicateM n (game strat start)
