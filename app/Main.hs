module Main where

import Data.Foldable (forM_)
import Data.Monoid
import qualified System.Environment as Sys
import Text.Printf

import MCTS (uniform_choose, take_obvious_plays, ucb1_choose, uct_choose)
import Expectimax (best_move)
import TicTacToe
import Types
import Umpire
import Talisman as Tal

results :: TicTacToe -> (Sum Int, Sum Int, Sum Int)
results g =
    case winner g of
      Right (Player 0) -> (Sum 1, Sum 0, Sum 0)
      Right (Player 1) -> (Sum 0, Sum 1, Sum 0)
      Left (Just ()) -> (Sum 0, Sum 0, Sum 1)
      Left Nothing -> (Sum 0, Sum 0, Sum 0)

win_probs :: Int -> TicTacToe -> IO ()
win_probs n g = render_evaluation (match n (versus [uct_choose 100 uniform_choose, ucb1_choose 100 uniform_choose]) results) g

benchmark :: Int -> Int -> Int -> IO ()
benchmark games budget1 budget2 = render_evaluation (match games strat results) (start :: TicTacToe) where
    strat = versus [tty_choose, (uct_choose budget2 take_obvious_plays)]

one_game :: Int -> IO ()
one_game budget = render_one_game (game strat) (start :: TicTacToe) where
    strat = versus [tty_choose, (uct_choose budget take_obvious_plays)]

do_one_game :: IO ()
do_one_game = do
  [arg1] <- Sys.getArgs
  one_game (read arg1)

do_benchmark :: IO ()
do_benchmark = do
  [arg1, arg2, arg3] <- Sys.getArgs
  benchmark (read arg1) (read arg2) (read arg3)

-- main = do_one_game
-- main = do_benchmark

main :: IO ()
main = do
  [max_time, max_lives, max_fate, max_strength] <- Sys.getArgs
  forM_ [6..(read max_time)]     (\time ->
   forM_ [1..(read max_lives)]    (\lives -> (do
    putStrLn $ show time ++ " turns " ++ show lives ++ " lives "
    putStr "fate"
    forM_ [1..(read max_strength)] (\strength -> printf " %2d str" (strength :: Int))
    putStrLn ""
    forM_ [0..(read max_fate)]     (\fate -> (do
     printf "%4d" fate
     forM_ [1..(read max_strength)] (\strength -> (do
      let (_, value) = best_move $ Tal.Board time (Tal.Status lives fate strength 0 0) Tal.SPortalOfPower
      printf " %5.2f%%" $ 100 * value))
     putStrLn "")))))
