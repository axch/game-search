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
import qualified Talisman as Tal

results :: TicTacToe -> (Sum Int, Sum Int, Sum Int)
results g =
    case winner g of
      Right Player1 -> (Sum 1, Sum 0, Sum 0)
      Right Player2 -> (Sum 0, Sum 1, Sum 0)
      Left (Just ()) -> (Sum 0, Sum 0, Sum 1)
      Left Nothing -> (Sum 0, Sum 0, Sum 0)

win_probs :: Int -> TicTacToe -> IO ()
win_probs n g = render_evaluation (match n (uct_choose 100 uniform_choose `versus` ucb1_choose 100 uniform_choose)  results) g

benchmark :: Int -> Int -> Int -> IO ()
benchmark games _budget1 budget2 = render_evaluation (match games strat results) (start :: TicTacToe) where
    strat = tty_choose `versus` uct_choose budget2 take_obvious_plays

one_game :: Int -> IO ()
one_game budget = render_one_game (game strat) (start :: TicTacToe) where
    strat = tty_choose `versus` uct_choose budget take_obvious_plays

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
  [max_time, max_lives, max_fate, in_strength, max_more, in_bonus] <- Sys.getArgs
  let strength = read in_strength
      bonus = read in_bonus
  putStrLn $ show strength ++ " base strength, " ++ show bonus ++ " combat bonus "
  forM_ [6..(read max_time)]     (\time ->
   forM_ [1..(read max_lives)]    (\lives -> (do
    putStr "str bon time life fate"
    forM_ [0..(read max_more)] (\more_strength -> printf " %1d mstr" (more_strength :: Int))
    putStrLn ""
    forM_ [0..(read max_fate)]     (\fate -> (do
     printf "%3d %3d %4d %4d %4d" strength bonus time lives fate
     forM_ [0..(read max_more)] (\more_strength -> (do
      let (_, value) = best_move $ Tal.Board time (Tal.Status lives fate strength more_strength bonus) Tal.SPortalOfPower
      printf " %5.2f%%" $ 100 * value))
     putStrLn "")))))
