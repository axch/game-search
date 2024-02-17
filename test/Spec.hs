{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (map)

import Control.Monad.State
import Data.Foldable (toList)
import qualified Data.Map as M
import System.Exit
import Test.HUnit
import Test.QuickCheck

import GameSearch.Expectimax (best_move, expectimax, visit_probabilities)
import GameSearch.Types (finished, r_move, moves)
import qualified GameSearch.Games.Heads as Heads
import qualified GameSearch.Games.Talisman as Tal
import TalismanTest()

test_single_result_1 :: Test
test_single_result_1 = test $ (value, M.size map) @?= (0.34770549572564535, 121525) where
    ((_move, value), map) = runState results M.empty
    results = expectimax $ Tal.Board 9 Tal.Strength start Tal.SPortalOfPower
    start = Tal.Status { Tal.lives = 5
                       , Tal.fate = 3
                       , Tal.base_strength = 2
                       , Tal.more_strength = 5
                       , Tal.combat_bonus = 0
                       , Tal.base_craft = 0
                       , Tal.more_craft = 0
                       }

test_single_result_2 :: Test
test_single_result_2 = test $ (value, M.size map) @?= (0.4590041950276001, 50074) where
    ((_move, value), map) = runState results M.empty
    results = expectimax $ Tal.Board 9 Tal.Craft start Tal.SPortalOfPower
    start = Tal.Status { Tal.lives = 5
                       , Tal.fate = 3
                       , Tal.base_strength = 2
                       , Tal.more_strength = 5
                       , Tal.combat_bonus = 0
                       , Tal.base_craft = 2
                       , Tal.more_craft = 5
                       }

prop_moves_shrink_state :: Tal.Board -> Property
prop_moves_shrink_state game@(Tal.Board time _ _ _) =
    time >= 1 ==> all (< game) options where
        options = concatMap (toList . (flip r_move game)) $ moves game

prop_solve_timed_heads :: Int -> Property
prop_solve_timed_heads n = n > 1 ==> (best_move $ Heads.TPlay n) == (Just (), 1 - (1/2)**(fromIntegral n))

prop_timed_heads_results :: Int -> Property
prop_timed_heads_results n = n > 1 ==> result == answer where
    result = M.filterWithKey (\g _ -> finished g) $ visit_probabilities $ Heads.TPlay n
    answer :: M.Map Heads.TimedHeads Double
    answer = M.fromList [(Heads.TWon, 1 - p_lose), (Heads.TPlay 0, p_lose)]
    p_lose = (1/2)**(fromIntegral n)

all_tests :: Test
all_tests = test [test_single_result_1, test_single_result_2]

return []
main :: IO ()
main = do
  results <- $quickCheckAll
  Counts { failures = f, errors = e } <- runTestTT all_tests
  if f + e == 0 && results then
      exitWith ExitSuccess
  else
      exitWith $ ExitFailure $ f + e + (if results then 0 else 1)
