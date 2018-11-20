module Main where

import Control.Monad.State
import qualified Data.Map as M
import Debug.Trace
import System.Exit
import Test.HUnit

import GameSearch.Expectimax (expectimax)
import qualified GameSearch.Games.Talisman as Tal

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

all_tests :: Test
all_tests = test [test_single_result_1, test_single_result_2]

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT all_tests
  if f + e == 0 then
      exitWith ExitSuccess
  else
      exitWith $ ExitFailure $ f + e
