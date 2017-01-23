module Umpire where

import GHC.Base (assert)

import Data.Random (RVar)

import Types

play_out :: (Game a m) => (a -> RVar m) -> a -> RVar a -- Where the returned state is terminal
play_out strat = go where
  go g | finished g = return g
       | otherwise = do m <- strat g
                        g' <- assert (valid m g) $ move m g
                        go g'
