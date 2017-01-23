module Main where

import Data.Monoid

import MCTS (uniform_choose, ucb1_choose)
import TicTacToe
import Types
import Umpire (match, versus, render_evaluation)

results :: TicTacToe -> (Sum Int, Sum Int, Sum Int)
results g =
    case winner g of
      Right (Player 0) -> (Sum 1, Sum 0, Sum 0)
      Right (Player 1) -> (Sum 0, Sum 1, Sum 0)
      Left (Just ()) -> (Sum 0, Sum 0, Sum 1)
      Left Nothing -> (Sum 0, Sum 0, Sum 0)

win_probs :: Int -> TicTacToe -> IO ()
win_probs n g = render_evaluation (match n (versus [ucb1_choose 100, ucb1_choose 1000]) results) g

main :: IO ()
main = undefined
