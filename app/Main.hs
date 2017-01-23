module Main where

import Data.Monoid

import Data.Random (RVar)

import MCTS (uniform_choose, ucb1_choose)
import TicTacToe
import Types
import Umpire (match)

results :: TicTacToe -> (Sum Int, Sum Int, Sum Int)
results g =
    case winner g of
      Right (Player 0) -> (Sum 1, Sum 0, Sum 0)
      Right (Player 1) -> (Sum 0, Sum 1, Sum 0)
      Left (Just ()) -> (Sum 0, Sum 0, Sum 1)
      Left Nothing -> (Sum 0, Sum 0, Sum 0)

win_probs :: Int -> TicTacToe -> IO ()
win_probs n g = render g >> sampleIO (match n (versus [ucb1_choose 100, ucb1_choose 1000]) results g) >>= (putStrLn . show)

doit :: (TicTacToe -> RVar TicMove) -> TicTacToe -> IO ()
doit strat g = do
  render g
  m <- sampleIO $ strat g
  g' <- sampleIO $ move m g
  render g'

versus :: (Game a m) => [(a -> RVar m)] -> a -> RVar m
versus strats g = (strats!!i) g where
    (Player i) = current g

main :: IO ()
main = undefined
