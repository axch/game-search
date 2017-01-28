{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module TicTacToe where

-- Player 0 is X
-- Player 1 is O

import Data.Bits
import Data.Char (ord)
import Data.Maybe
import Text.Printf (printf)

import Types

-- Configuration

board_width :: Int
board_width = 7

board_height :: Int
board_height = 7

win_length :: Int
win_length = 5

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
    known_one_move_wins = one_move_wins
    known_one_move_blocks = one_move_win_blocks

-- Optimizations

-- Filter the win masks by non-intersection with the opponent's
-- positions.
available_masks :: Mask -> [Mask]
available_masks opp = filter available win_masks where
    available mask = mask .&. opp == zeroBits
{-# INLINE available_masks #-}

one_off :: Mask -> Mask -> Maybe Mask
one_off present needed = if popCount candidate == 1 then Just candidate else Nothing
    where candidate = needed .&. (complement present)
{-# INLINE one_off #-}

one_move_win_masks :: TicTacToe -> [Mask]
one_move_win_masks (TicTacToe (Player 0) xs os) =
    catMaybes $ map (one_off xs) $ available_masks os
one_move_win_masks (TicTacToe (Player 1) xs os) =
    catMaybes $ map (one_off os) $ available_masks xs

one_move_wins :: TicTacToe -> [TicMove]
one_move_wins g@(TicTacToe p _ _) = map (TicMove p) $ one_move_win_masks g

one_move_win_blocks :: TicTacToe -> [TicMove]
one_move_win_blocks (TicTacToe p xs os) =
    map (TicMove p) $ one_move_win_masks $ TicTacToe (opponent p) xs os

-- Debugging

render_tic_tac_toe :: TicTacToe -> String
render_tic_tac_toe (TicTacToe _ xs os) = (concat $ map row [0..(board_height - 1)]) ++ bottom where
    row i = (printf "%2d " (i+1)) ++ map cell [board_width*i..(board_width*(i+1) - 1)] ++ "\n"
    cell i | testBit xs i = 'X'
           | testBit os i = 'O'
           | otherwise = '.'
    bottom = "   " ++ take board_width ['a','b'..]

instance Renderable TicTacToe where
    render = putStrLn . render_tic_tac_toe

winning_boards :: [TicTacToe]
winning_boards = map (TicTacToe undefined zeroBits) win_masks

move_at :: (Int, Int) -> TicTacToe -> TicTacToe
move_at loc g = tic_move m g where
    m = TicMove (current g) $ bit $ coords_to_bit_loc loc

moves_at :: [(Int, Int)] -> TicTacToe -> TicTacToe
moves_at locs g = foldl (flip move_at) g locs

instance Renderable TicMove where
    render m = render (tic_move m start)

instance CtxParseable TicTacToe TicMove where
    ctx_parse _ "" = Left "Blank move"
    ctx_parse g (col:rows) = Right $ TicMove (current g) $ bit $ coords_to_bit_loc (r, c) where
                                        c = ord col - ord 'a'
                                        r = read rows - 1
                                        -- TODO check for being in the board and not being on top of a piece?
