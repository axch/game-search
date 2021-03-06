module Main where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Foldable (forM_)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as M
import qualified System.Environment as Sys
import Text.Printf

import qualified GameSearch.Expectimax as Exp
import qualified GameSearch.Games.Talisman as Tal

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

type Strategy = M.Map Tal.Board (Maybe Tal.Move, Double)
type Strategic a = StateT Strategy IO a

print_block :: Tal.SmallInt -> Tal.SmallInt -> Tal.SmallInt -> Tal.SmallInt -> Tal.SmallInt -> [Tal.SmallInt] -> [Tal.SmallInt] -> [Tal.SmallInt] -> Strategic ()
print_block strength craft bonus time lives fates more_strengths more_crafts = do
  lift $ putStr "str cft bon time life fate"
  forM_ more_strengths (\more_strength -> (do
   forM_ more_crafts (\more_craft -> lift $ printf "  +%1d/+%1d" more_strength more_craft)))
  lift $ putStrLn ""
  forM_ fates (\fate -> (do
   lift $ printf "%3d %3d %3d %4d %4d %4d" strength craft bonus time lives fate
   forM_ more_strengths (\more_strength -> (do
    forM_ more_crafts (\more_craft -> (do
     let status = Tal.Status lives fate strength more_strength bonus craft more_craft
     (_, value) <- mapStateT (return . runIdentity) $ Exp.expectimax $ Tal.Board time Tal.Strength status Tal.SPortalOfPower
     lift $ printf " %5.2f%%" $ 100 * value))))
   lift $ putStrLn ""))

run :: [String] -> IO ()
run args = do
  let [max_time, max_lives, max_fate, in_strength, max_more_str, in_bonus, in_craft, max_more_cft] = args
  let strength = read in_strength
      craft = read in_craft
      bonus = read in_bonus
  putStrLn $ show strength ++ " base strength, " ++ show craft ++ " base craft, " ++ show bonus ++ " combat bonus "
  evalStateT (
   forM_ [6..(read max_time)] (\time ->
    forM_ [1..(read max_lives)] (\lives ->
     print_block strength craft bonus time lives [0..(read max_fate)] [0..(read max_more_str)] [0..(read max_more_cft)])))
   M.empty

main :: IO ()
main = do
  args <- Sys.getArgs
  if length args == 8 then
      run args
  else
      print_usage_and_exit
