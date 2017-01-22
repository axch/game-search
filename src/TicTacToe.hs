{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TicTacToe where

-- Player 0 is X
-- Player 1 is O

import Data.Bits

import Data.Random (RVar)

import Types

type Mask = Int

data TicTacToe = TicTacToe Player Mask Mask -- Mask of spaces each player occupies
  deriving (Eq, Show)

data TicMove = TicMove Player Mask -- Mask to .|. with that player's spaces; should be a singleton
  deriving (Eq, Show)

instance Move TicMove where

move_masks :: [Mask]
move_masks = map bit [0..8]

tic_moves :: TicTacToe -> [TicMove]
tic_moves (TicTacToe p xs os) = map (TicMove p) $ filter place_ok move_masks
  where
    place_ok mask = popCount (mask .&. (xs .|. os)) == 0

opponent :: Player -> Player
opponent (Player 0) = Player 1
opponent (Player 1) = Player 0

-- TODO: The eternal dilemma: to check or to assume the validity of
-- the move?  I suppose I will want the unchecked version...
tic_move :: TicMove -> TicTacToe -> RVar TicTacToe
tic_move (TicMove (Player 0) m) (TicTacToe p' xs os) =
    return $ TicTacToe (opponent p') (xs .|. m) os
tic_move (TicMove (Player 1) m) (TicTacToe p' xs os) =
    return $ TicTacToe (opponent p') xs (os .|. m)

win_masks :: [Mask]
win_masks = [ 0b000000111
            , 0b000111000
            , 0b111000000
            , 0b001001001
            , 0b010010010
            , 0b100100100
            , 0b100010001
            , 0b001010100
            ]

mask_contains :: Mask -> Mask -> Bool
mask_contains m1 m2 = m1 == m1 .&. m2

winner :: TicTacToe -> Either (Maybe ()) Player -- Left $ Just () means 'draw'
winner (TicTacToe _ xs os)
    | any (flip mask_contains xs) win_masks = Right $ Player 0
    | any (flip mask_contains os) win_masks = Right $ Player 1
    | popCount (xs .|. os) == 9 = Left $ Just () -- Draw
    | otherwise = Left Nothing

instance Game TicTacToe TicMove where
    moves = tic_moves
    move = tic_move
    start = TicTacToe (Player 0) zeroBits zeroBits
    finished g = not (winner g == Left Nothing)
    payoff g = assess where
        win = winner g
        assess p | win == Right p = Just 1
                 | win == Left Nothing = Nothing
                 | otherwise = Just 0
    current (TicTacToe p _ _) = p

render_tic_tac_toe :: TicTacToe -> String
render_tic_tac_toe (TicTacToe _ xs os) = concat $ map row [0..2] where
    row i = map cell [3*i..2+3*i] ++ "\n"
    cell i | testBit xs i = 'X'
           | testBit os i = 'O'
           | otherwise = '.'

render :: TicTacToe -> IO ()
render = putStrLn . render_tic_tac_toe
