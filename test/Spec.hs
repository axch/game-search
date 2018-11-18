module Main where

import Debug.Trace
import System.Exit
import Test.HUnit

import GameSearch.Expectimax (best_move)
import qualified GameSearch.Games.Talisman as Tal

test_single_result :: Test
test_single_result = test $ result @?= (Just Tal.Proceed, 0.34770549572564535) where
    result = best_move $ Tal.Board 9 start Tal.SPortalOfPower
    start = Tal.Status { Tal.lives = 5
                       , Tal.fate = 3
                       , Tal.base_strength = 2
                       , Tal.more_strength = 5
                       , Tal.combat_bonus = 0
                       , Tal.base_craft = 0
                       , Tal.more_craft = 0
                       }

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT test_single_result
  if f + e == 0 then
      exitWith ExitSuccess
  else
      exitWith $ ExitFailure $ f + e
