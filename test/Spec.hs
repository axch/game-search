{-# LANGUAGE TemplateHaskell #-}

module Main where

import Debug.Trace
import System.Exit
import Test.QuickCheck

import GameSearch.Expectimax (best_move)
import qualified GameSearch.Games.Talisman as Tal

prop_single_result :: Bool
prop_single_result = result == (Just Tal.Proceed, 0.3477) where
    result = best_move $ Tal.Board 9 start Tal.SPortalOfPower
    start = Tal.Status { Tal.lives = 5
                       , Tal.fate = 3
                       , Tal.base_strength = 2
                       , Tal.more_strength = 5
                       , Tal.combat_bonus = 0
                       , Tal.base_craft = 0
                       , Tal.more_craft = 0
                       }

return []

main :: IO ()
main = do
  results <- $quickCheckAll
  if results then
      exitWith ExitSuccess
  else
      exitWith $ ExitFailure 1
