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

import Data.Foldable (forM_)
import Data.Monoid
import qualified System.Environment as Sys
import Text.Printf

import GameSearch.MCTS (uniform_choose, take_obvious_plays, ucb1_choose, uct_choose)
import GameSearch.Expectimax (best_move)
import GameSearch.Games.TicTacToe
import GameSearch.Types
import GameSearch.Umpire
import qualified GameSearch.Games.Talisman as Tal

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
benchmark games _budget1 budget2 = render_evaluation (match games strat results) start where
    strat = tty_choose `versus` uct_choose budget2 take_obvious_plays

one_game :: Int -> IO ()
one_game budget = render_one_game (game strat) start where
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

print_usage_and_exit :: IO ()
print_usage_and_exit = do
  putStrLn "Usage: talisman-probabilities max_time_limit max_lives max_fate base_strength max_more_strength combat_bonus base_craft max_more_craft"
  putStrLn ""
  putStrLn "Computes probabilities of reaching the Valley of Fire in the Talisman endgame"
  putStrLn "- in time turns for time between 6 and max_time_limit"
  putStrLn "- starting with 1 to max_lives lives"
  putStrLn "- starting with 0 to max_fate Fate"
  putStrLn "- with base_strength Strength from character sheet, Objects, and Followers"
  putStrLn "- with 0 to max_more_strength additional Strength previously earned"
  putStrLn "- with combat_bonus additional strength for combat only"
  putStrLn "- with base_craft Craft from character sheet, Objects, and Followers"
  putStrLn "- with 0 to max_more_craft additional Craft previously earned."

run_talisman :: [String] -> IO ()
run_talisman args = do
  let [max_time, max_lives, max_fate, in_strength, max_more_str, in_bonus, in_craft, max_more_cft] = args
  let strength = read in_strength
      craft = read in_craft
      bonus = read in_bonus
  putStrLn $ show strength ++ " base strength, " ++ show craft ++ " base craft, " ++ show bonus ++ " combat bonus "
  forM_ [6..(read max_time)]     (\time ->
   forM_ [1..(read max_lives)]    (\lives -> (do
    putStr "str cft bon time life fate"
    forM_ [0..(read max_more_str)] (\more_strength -> (do
     forM_ [0..(read max_more_cft)] (\more_craft -> printf "  +%1d/+%1d" (more_strength :: Int) (more_craft :: Int))))
    putStrLn ""
    forM_ [0..(read max_fate)]     (\fate -> (do
     printf "%3d %3d %3d %4d %4d %4d" strength craft bonus time lives fate
     forM_ [0..(read max_more_str)] (\more_strength -> (do
      forM_ [0..(read max_more_cft)] (\more_craft -> (do
       let status = Tal.Status lives fate strength more_strength bonus craft more_craft
       let (_, value) = best_move $ Tal.Board time status Tal.SPortalOfPower
       printf " %5.2f%%" $ 100 * value))))
     putStrLn "")))))

main :: IO ()
main = do
  args <- Sys.getArgs
  if length args == 8 then
      run_talisman args
  else
      print_usage_and_exit
