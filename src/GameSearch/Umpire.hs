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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GameSearch.Umpire where

-- Running games and tournaments, including against a human player.

import Control.Monad
import GHC.Base (assert)
import System.IO

import Data.Random (MonadRandom)

import GameSearch.Types

tty_choose :: (Game a m, CtxParseable a m, Show (Player a)) => a -> IO m
tty_choose g = do
  render g
  putStr $ "You are " ++ show (current g) ++ " > "
  hFlush stdout
  ans <- getLine
  case ctx_parse g ans of
    Left msg -> do
      putStrLn "Your move did not parse"
      putStrLn msg
      putStrLn "Try again"
      tty_choose g
    Right m -> if valid m g then
                   return m
               else do
                 putStrLn "Your move was invalid"
                 putStrLn "Try again"
                 tty_choose g

render_one_move :: (Game a m) => (a -> IO m) -> a -> IO ()
render_one_move strat g = do
  render g
  m <- strat g
  render $ move m g

render_one_game :: (Renderable a) => (a -> IO a) -> a -> IO ()
render_one_game strat g = do
  render g
  g' <- strat g
  render g'

render_evaluation :: (Renderable a, Show m) => (a -> IO m) -> a -> IO ()
render_evaluation eval g = do
  render g
  res <- eval g
  putStrLn $ show res

versus :: (Game a m, Player a ~ TwoPlayer) => (a -> b) -> (a -> b) -> a -> b
versus strat1 strat2 g = case current g of
                           Player1 -> strat1 g
                           Player2 -> strat2 g
{-# INLINE versus #-}

game :: (Game a m, MonadRandom r) => (a -> r m) -> a -> r a -- Where the returned state is terminal
game strat = go where
  go g | finished g = return g
       | otherwise = do m <- strat g
                        go $ assert (valid m g) $ move m g
{-# SPECIALIZE game :: (Game a m) => (a -> IO m) -> a -> IO a #-}

match :: (Monoid res, Game a m, MonadRandom r) => Int -> (a -> r m) -> (a -> res) -> a -> r res
match n strat eval start =
    liftM mconcat $ liftM (map eval) $ replicateM n (game strat start)
{-# SPECIALIZE match :: (Monoid res, Game a m) => Int -> (a -> IO m) -> (a -> res) -> a -> IO res #-}