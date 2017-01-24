module Umpire where

import Control.Monad
import GHC.Base (assert)

import Types

render_one_move :: (Game a m) => (a -> Ran m) -> a -> IO ()
render_one_move strat g = do
  render g
  m <- sampleIO $ strat g
  g' <- sampleIO $ move m g
  render g'

render_one_game :: (Renderable a) => (a -> Ran a) -> a -> IO ()
render_one_game strat g = do
  render g
  g' <- sampleIO $ strat g
  render g'

render_evaluation :: (Renderable a, Show m) => (a -> Ran m) -> a -> IO ()
render_evaluation eval g = do
  render g
  res <- sampleIO $ eval g
  putStrLn $ show res

versus :: (Game a m) => [(a -> Ran m)] -> a -> Ran m
versus strats g = (strats!!i) g where
    (Player i) = current g

game :: (Game a m) => (a -> Ran m) -> a -> Ran a -- Where the returned state is terminal
game strat = go where
  go g | finished g = return g
       | otherwise = do m <- strat g
                        g' <- assert (valid m g) $ move m g
                        go g'

match :: (Monoid res, Game a m) => Int -> (a -> Ran m) -> (a -> res) -> a -> Ran res
match n strat eval start =
    liftM mconcat $ liftM (map eval) $ replicateM n (game strat start)
