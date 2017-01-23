module Main where

import Data.Monoid

import Data.Random (RVar)

import MCTS (uniform_choose)
import TicTacToe
import Types
import Umpire (match)

payoffs :: TicTacToe -> (Sum Double, Sum Double)
payoffs g = (Sum p1, Sum p2) where
    win = payoff g
    (Just p1) = win $ Player 0
    (Just p2) = win $ Player 1


win_probs :: Int -> TicTacToe -> IO ()
win_probs n g = render g >> sampleIO (match n uniform_choose payoffs g) >>= (putStrLn . show)

doit :: (TicTacToe -> RVar TicMove) -> TicTacToe -> IO ()
doit strat g = do
  render g
  m <- sampleIO $ strat g
  g' <- sampleIO $ move m g
  render g'

main :: IO ()
main = undefined
