{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module TicTacToe where

-- Player 0 is X
-- Player 1 is O

import Data.Bits
import Data.Maybe

import Types

-- Configuration

board_width :: Int
board_width = 5

board_height :: Int
board_height = 5

win_length :: Int
win_length = 4

-- Computed configuration

type Mask = Int

board_size :: Int
board_size = board_width * board_height

move_masks :: [Mask]
move_masks = map bit [0..(board_size - 1)]

rows :: [Int]
rows = [0..(board_height-1)]

cols :: [Int]
cols = [0..(board_width-1)]

cells :: [(Int, Int)]
cells = concatMap (\r -> map (r,) cols) rows

coords_to_bit_loc :: (Int, Int) -> Int
coords_to_bit_loc (r, c) = r * board_width + c

in_board :: Int -> Int -> Bool
in_board r c = 0 <= r && r < board_height && 0 <= c && c < board_width

win_cells :: [[(Int, Int)]]
win_cells = concatMap candidates cells where
    candidates cell = catMaybes $ map (candidate win_length cell) deltas
    deltas = [(0, 1), (1, 0), (1, 1), (1, -1)]
    candidate 1 (r, c) (_, _)   | in_board r c = Just [(r, c)]
                                | otherwise = Nothing
    candidate k (r, c) (dr, dc) = case candidate (k-1) (r + dr, c + dc) (dr, dc) of
                                    Just places -> Just $ (r, c):places
                                    Nothing -> Nothing

win_masks :: [Mask]
win_masks = map enmask win_cells where
    enmask = (foldl (.|.) zeroBits) . (map bit) . (map coords_to_bit_loc)

-- Implementation

data TicTacToe = TicTacToe Player Mask Mask -- Mask of spaces each player occupies
  deriving (Eq, Show)

data TicMove = TicMove Player Mask -- Mask to .|. with that player's spaces; should be a singleton
  deriving (Eq, Ord, Show)

instance Move TicMove where

place_ok :: TicTacToe -> Mask -> Bool
place_ok (TicTacToe _ xs os) mask = (mask .&. (xs .|. os)) == zeroBits

tic_moves :: TicTacToe -> [TicMove]
tic_moves g@(TicTacToe p _ _) = map (TicMove p) $ filter (place_ok g) move_masks

opponent :: Player -> Player
opponent (Player 0) = Player 1
opponent (Player 1) = Player 0

-- TODO: The eternal dilemma: to check or to assume the validity of
-- the move?  I suppose I will want the unchecked version...
tic_move :: TicMove -> TicTacToe -> TicTacToe
tic_move (TicMove (Player 0) m) (TicTacToe p' xs os) =
    TicTacToe (opponent p') (xs .|. m) os
tic_move (TicMove (Player 1) m) (TicTacToe p' xs os) =
    TicTacToe (opponent p') xs (os .|. m)

valid_tic_move :: TicMove -> TicTacToe -> Bool
valid_tic_move (TicMove p m) g@(TicTacToe p' _ _) = p == p' && place_ok g m

mask_contains :: Mask -> Mask -> Bool
mask_contains m1 m2 = m1 == m1 .&. m2

winner :: TicTacToe -> Either (Maybe ()) Player -- Left $ Just () means 'draw'
winner (TicTacToe _ xs os)
    | any (flip mask_contains xs) win_masks = Right $ Player 0
    | any (flip mask_contains os) win_masks = Right $ Player 1
    | popCount (xs .|. os) == board_size = Left $ Just () -- Draw
    | otherwise = Left Nothing

instance Game TicTacToe TicMove where
    moves = tic_moves
    move m = return . tic_move m
    valid = valid_tic_move
    start = TicTacToe (Player 0) zeroBits zeroBits
    finished g = not (winner g == Left Nothing)
    payoff g = assess where
        win = winner g
        assess p | win == Right p = Just 1
                 | win == Left Nothing = Nothing
                 | win == Left (Just ()) = Just 0.5
                 | otherwise = Just 0
    current (TicTacToe p _ _) = p

-- Debugging

render_tic_tac_toe :: TicTacToe -> String
render_tic_tac_toe (TicTacToe _ xs os) = concat $ map row [0..(board_height - 1)] where
    row i = map cell [board_width*i..(board_width*(i+1) - 1)] ++ "\n"
    cell i | testBit xs i = 'X'
           | testBit os i = 'O'
           | otherwise = '.'

instance Renderable TicTacToe where
    render = putStrLn . render_tic_tac_toe

winning_boards :: [TicTacToe]
winning_boards = map (TicTacToe undefined zeroBits) win_masks
