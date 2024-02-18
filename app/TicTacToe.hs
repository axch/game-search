-- Copyright 2017 Alexey Radul

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main where

-- Executable driver.  Tweak `main` and recompile to change function.

import Data.Monoid
import qualified System.Environment as Sys

import GameSearch.MCTS (uniform_choose, take_obvious_plays, ucb1_choose, uct_choose)
import GameSearch.Games.TicTacToe
import GameSearch.Types
import GameSearch.Umpire

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
benchmark games budget1 budget2 = render_evaluation (match games strat results) start where
    strat = uct_choose budget1 (take_obvious_plays uniform)
            `versus` uct_choose budget2 (take_obvious_plays uniform)

one_game :: Int -> IO ()
one_game budget = render_one_game (game strat) start where
    strat = tty_choose
            `versus` uct_choose budget (take_obvious_plays uniform_choose)

do_one_game :: IO ()
do_one_game = do
  [arg1] <- Sys.getArgs
  one_game (read arg1)

do_benchmark :: IO ()
do_benchmark = do
  [arg1, arg2, arg3] <- Sys.getArgs
  benchmark (read arg1) (read arg2) (read arg3)

main :: IO ()
-- main = do_one_game
main = do_benchmark
