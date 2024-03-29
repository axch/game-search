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

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module GameSearch.Games.TicTacToe where

-- Generalized tic-tac-toe, admitting rectangular boards of arbitrary
-- size (up to 63 squares) and arbitrary-length straight paths needed
-- to win.

-- This class of games turns out to have a name and a mathematical
-- community interested in it: https://en.wikipedia.org/wiki/M,n,k-game

-- Player1 is Xs
-- Player2 is Os

import Data.Bits
import Data.Bits.Pdep
import Data.Char (ord)
import Data.Maybe
import GHC.Word
import Text.Printf (printf)

import GameSearch.Types hiding (Player)
import qualified GameSearch.Types as Types

type Player = Types.TwoPlayer

-- Bit twiddling

-- Return a mask holding the `i`th set bit of the second argument
select_mask :: Int -> Mask -> Mask
select_mask i m = pdep (1 .<<. i) m

-- Configuration

-- For the sake of performance, the board size and win condition are
-- hardcoded here, and will need to be edited to experiment with other
-- boards.  Everything else is derived from these constants.

board_width :: Int
board_width = 7

board_height :: Int
board_height = 7

win_length :: Int
win_length = 5

-- Computed configuration

type Mask = Word64

board_size :: Int
board_size = board_width * board_height

board_mask :: Mask
board_mask = bit board_size - 1

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

place_ok :: TicTacToe -> Mask -> Bool
place_ok (TicTacToe _ xs os) mask = (mask .&. (xs .|. os)) == zeroBits

tic_moves :: TicTacToe -> [TicMove]
tic_moves g@(TicTacToe p _ _) = map (TicMove p) $ filter (place_ok g) move_masks
{-# SCC tic_moves #-}

opponent :: Player -> Player
opponent Player1 = Player2
opponent Player2 = Player1

-- TODO: The eternal dilemma: to check or to assume the validity of
-- the move?  I suppose I will want the unchecked version...
tic_move :: TicMove -> TicTacToe -> TicTacToe
tic_move (TicMove Player1 m) (TicTacToe p' xs os) =
    TicTacToe (opponent p') (xs .|. m) os
tic_move (TicMove Player2 m) (TicTacToe p' xs os) =
    TicTacToe (opponent p') xs (os .|. m)

valid_tic_move :: TicMove -> TicTacToe -> Bool
valid_tic_move (TicMove p m) g@(TicTacToe p' _ _) = p == p' && place_ok g m

mask_contains :: Mask -> Mask -> Bool
mask_contains m1 m2 = m1 == m1 .&. m2

winner :: TicTacToe -> Either (Maybe ()) Player -- Left $ Just () means 'draw'
winner (TicTacToe _ xs os)
    | any (flip mask_contains xs) win_masks = Right $ Player1
    | any (flip mask_contains os) win_masks = Right $ Player2
    | popCount (xs .|. os) == board_size = Left $ Just () -- Draw
    | otherwise = Left Nothing
{-# SCC winner #-}

instance RGame TicTacToe TicMove where
    type Player TicTacToe = TwoPlayer
    moves = tic_moves
    {-# INLINE moves #-}
    r_move = default_r_move
    {-# INLINE r_move #-}
    valid = valid_tic_move
    {-# INLINE valid #-}
    finished g = not (winner g == Left Nothing)
    {-# INLINE finished #-}
    payoff g = assess where
        win = winner g
        assess p | win == Right p = Just 1
                 | win == Left Nothing = Nothing
                 | win == Left (Just ()) = Just 0.5
                 | otherwise = Just 0
    {-# INLINE payoff #-}
    current (TicTacToe p _ _) = p
    {-# INLINE current #-}
    known_one_move_wins = one_move_wins
    {-# INLINE known_one_move_wins #-}
    known_one_move_blocks = one_move_win_blocks
    {-# INLINE known_one_move_blocks #-}

instance Game TicTacToe TicMove where
    move m = tic_move m
    {-# INLINE move #-}

start :: TicTacToe
start = TicTacToe Player1 zeroBits zeroBits

-- Optimization: Find winning moves by scanning wins rather than moves

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
one_move_win_masks (TicTacToe Player1 xs os) =
    catMaybes $ map (one_off xs) $ available_masks os
one_move_win_masks (TicTacToe Player2 xs os) =
    catMaybes $ map (one_off os) $ available_masks xs

one_move_wins :: TicTacToe -> [TicMove]
one_move_wins g@(TicTacToe p _ _) = map (TicMove p) $ one_move_win_masks g
{-# SCC one_move_wins #-}

one_move_win_blocks :: TicTacToe -> [TicMove]
one_move_win_blocks (TicTacToe p xs os) =
    map (TicMove p) $ one_move_win_masks $ TicTacToe (opponent p) xs os
{-# SCC one_move_win_blocks #-}

-- Optimization: Select a random move by representing moves as a mask, not [TicMove]

type TicMoves = (TwoPlayer, Mask)

tic_move_set :: TicTacToe -> TicMoves
tic_move_set (TicTacToe p xs os) = (p, board_mask .&. complement (xs .|. os))

uniform :: (MonadUnifRandom r) => TicTacToe -> r TicMove
uniform g = do
  let (p, mvs) = tic_move_set g
  let n_moves = popCount mvs
  index <- sample 0 (n_moves - 1)
  return $ TicMove p $ select_mask index mvs
{-# SCC uniform #-}

-- Debugging

render_tic_tac_toe :: TicTacToe -> String
render_tic_tac_toe (TicTacToe _ xs os) = top ++ middle ++ bottom where
    middle = concat $ map row [0..(board_height - 1)]
    row i = (printf "%2d " (i+1)) ++ map cell [board_width*i..(board_width*(i+1) - 1)] ++ "\n"
    cell i | testBit xs i = 'X'
           | testBit os i = 'O'
           | otherwise = '.'
    bottom = "   " ++ take board_width ['a','b'..]
    top = "Get " ++ show win_length ++ " in a row\n"

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
    ctx_parse g (col_c:row_cs) = Right $ TicMove (current g) $ bit $ coords_to_bit_loc (r, c) where
                                        c = ord col_c - ord 'a'
                                        r = read row_cs - 1
                                        -- TODO check for being in the board and not being on top of a piece?
