module Main where

import Data.Monoid
import qualified System.Environment as Sys

import MCTS (uniform_choose, take_obvious_plays, ucb1_choose, uct_choose)
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
win_probs n g = render_evaluation (match n (versus [uct_choose 100 uniform_choose, ucb1_choose 100 uniform_choose]) results) g

benchmark :: Int -> Int -> IO ()
benchmark games budget = render_evaluation (match games strat results) (start :: TicTacToe) where
    strat = versus [(uct_choose budget take_obvious_plays), (uct_choose budget take_obvious_plays)]

main :: IO ()
main = do
  [arg1, arg2] <- Sys.getArgs
  benchmark (read arg1) (read arg2)
