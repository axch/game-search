module Main where

import Control.Monad
import Data.Monoid

import Data.Random (RVar)

import MCTS (play_out)
import TicTacToe
import Types

payoffs :: TicTacToe -> (Sum Double, Sum Double)
payoffs g = (Sum p1, Sum p2) where
    win = payoff g
    (Just p1) = win $ Player 0
    (Just p2) = win $ Player 1


accumulate :: (Monoid m) => Int -> (TicTacToe -> m) -> TicTacToe -> RVar m
accumulate n f s = liftM mconcat $ liftM (map f) $ replicateM n (play_out s)

doit :: Int -> TicTacToe -> IO ()
doit n g = render g >> sampleIO (accumulate n payoffs g) >>= (putStrLn . show)

main :: IO ()
main = undefined
