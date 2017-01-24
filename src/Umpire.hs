module Umpire where

import Control.Monad
import GHC.Base (assert)

import Data.Random (MonadRandom)

import Types

render_one_move :: (Game a m) => (a -> IO m) -> a -> IO ()
render_one_move strat g = do
  render g
  m <- strat g
  g' <- move m g
  render g'

render_one_game :: (Renderable a) => (a -> IO a) -> a -> IO ()
render_one_game strat g = do
  render g
  g' <- strat g
  render g'

render_evaluation :: (Renderable a, Show m) => (a -> IO m) -> a -> IO ()
render_evaluation eval g = do
  render g
  res <- eval g
  putStrLn $ show res

versus :: (Game a m) => [(a -> b)] -> a -> b
versus strats g = (strats!!i) g where
    (Player i) = current g

game :: (Game a m, MonadRandom r) => (a -> r m) -> a -> r a -- Where the returned state is terminal
game strat = go where
  go g | finished g = return g
       | otherwise = do m <- strat g
                        g' <- assert (valid m g) $ move m g
                        go g'

match :: (Monoid res, Game a m, MonadRandom r) => Int -> (a -> r m) -> (a -> res) -> a -> r res
match n strat eval start =
    liftM mconcat $ liftM (map eval) $ replicateM n (game strat start)
