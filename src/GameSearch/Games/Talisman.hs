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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module GameSearch.Games.Talisman where

-- A partial implementation of the endgame of the board game Talisman,
-- currently published by Fantasy Flight Games.
-- https://www.fantasyflightgames.com/en/products/talisman/

-- This is interesting because
-- a) Attempting to get from the Portal of Power to the Crown of
--    Command is almost exactly a (stochastic) single-player
--    solitaire.
-- b) Its state space is small enough to be amenable to exhaustive
--    search, but large enough that one needs a computer to carry this
--    search out.
-- c) Choosing when to attempt it is a significant decision in one's
--    play of the overall game.

-- So far, this is a reasonable model of the Crypt-Death-Werewolf and
-- the Mine-Vampire-Pits forks as separate end-games, but without
-- allowing dynamic choice of path.

-- TODO expand the model with:
-- + The craft attribute
-- + The Mine-Vampire-Pits fork
-- + Run-time control of whether one is studying the craft path,
--   strength path, or full game.  This can be implemented as a field of
--   Board.
-- - Choosing which fork to take, including after failing the
--   crypt/mine (not that one ever wants to change one's mind about
--   that).
-- - Choosing whether to open the Portal with craft (as opposed to strength)
-- - Modeling the outside world, for variable results from the crypt/mine
-- - Modeling the gnome or dwarf ability
-- - Detail: Monk's combat bonus depends on their bonus craft, so can change
-- - Detail: Warrior rolls an extra die during combat, which changes
--   win probs and usefulness of fate
-- - Bug: Double-check whether one is allowed to use fate to make the
--   Werewolf (or Pit Fiends) re-roll, and implement if so

-- TODO (performance)
-- + Since I know which die is best to reroll in any situation, I can
--   reduce the number of situations that need to be considered by
--   collapsing out die order, and only allowing one "Reroll" move.
--   - Dice with Death moves from 3 * 6^4 = 3888 states
--     to 21 * 21 + 2 * 11 * 21 = 903 states
--   - Crypt/Mine 6^3 to 36 each
--   - FightWerewolf 11 * 36 to 16 * 6 (unless can reroll both dice)
--   - FightPitFiends 6^3 to 6^3
--   - Ergo, this should be worth a 3x reduction in the search space
-- - Strictifying things should help
--   - Maybe put in a Criterion benchmark of an easy run before doing this?
--     - http://www.serpentine.com/criterion/tutorial.html
-- + I can reuse the move cache across multiple top-level position
--   evaluations in the main driver.
-- - However, keeping the move cache around can lead to avoidable OOM
--   on large parameter sweeps.
-- - After the character passes the Crypt or the Mine, we can (a)
--   forget their Craft attributes completely, and (b) fold their
--   more_strength and combat_bonus into their base_strength, because
--   those distinctions no longer matter.  This should drastically
--   speed up high-time high-more_craft runs, as well as parameter
--   sweeps that vary base attributes, because all the heavy board
--   positions are after the Crypt and the Mine.
-- + We can abort early when we know the character doesn't have enough
--   time.  0 turns at the ValleyOfFire lose, so 1 turn at PitFiends
--   or Werewolf lose, so 2 turns at Vampire or DiceWithDeath lose, so
--   3 turns at Crypt or Mine lose, so 4 turns at PlainOfPeril lose,
--   so 5 turns at PortalOfPower lose.  For a total run of 13 turns,
--   DiceWithDeath (which is the heavy one before stat collapse) sees
--   a 20% reduction in viable game states.
-- - We can fast-path the 0 fate case by auto-Proceeding on all the
--   non-S nodes, thus avoiding materializing them at all.  (It's
--   important not to auto-Proceed the S nodes, so that the
--   probabilities all re-collapse.)  For a 4-fate run, this could
--   save ~20% of states, and might be useful as a fast auto-solve
--   for an MCTS-based variant.
--   - The game-independent version of this is "don't cache positions
--     where neither the player nor chance make any choices before the
--     next position you would cache".  This would be useful when the
--     obligatory move(s) are irreversible, by reducing the number of
--     positions that need caching, and hence the memory footprint.
--   - To gain a speed benefit from this insight, should auto-add
--     intermediate rolling results, and insert Probabilities
--     reductions.  That way, instead of exploring all 6^4 possible
--     roll sets for DiceWithDeath, could explore just 11^2; or maybe
--     even pre-compute the difference distribution and explore only
--     the 3 outcomes.  The value of this obviously depends on how
--     many times SDiceWithDeath is expanded.
--   - Possible implementation: Fork the whole game into "has Fate"
--     and "does not have Fate" branches, and code them essentially
--     separately.

import GHC.Generics (Generic)
import Data.Word (Word8)

import GameSearch.Types

type SmallInt = Word8
type Die = SmallInt

-- Positions prefixed with "S" are starting points, where one has not
-- yet rolled any dice for the space.
data Position = ValleyOfFire
              | FightWerewolf SmallInt Die Die
              | SFightWerewolf SmallInt
              | Werewolf Die Die
              | SWerewolf
              | DiceWithDeathA SmallInt Die Die -- Rerolled one of mine first
              | DiceWithDeathB Die Die SmallInt -- Rerolled one of Death's first
              | DiceWithDeath Die Die Die Die
              | SDiceWithDeath
              | Crypt Die SmallInt
              | SCrypt
              | FightPitFiends SmallInt Combat
              | PitFiends Die
              | SPitFiends
              | Vampire Die
              | SVampire
              | Mine Die SmallInt -- The int is the sum of the lower two dice
              | SMine
              | PlainOfPeril
              | PortalOfPower Die Die
              | SPortalOfPower
  deriving (Eq, Ord, Show, Generic)

data Combat = Fight Die Die
            | Start
  deriving (Eq, Ord, Show, Generic)

data Status = Status
    { lives :: SmallInt
    , fate :: SmallInt
    , base_strength :: SmallInt
    , more_strength :: SmallInt
    , combat_bonus :: SmallInt
    , base_craft :: SmallInt
    , more_craft :: SmallInt
    }
  deriving (Eq, Ord, Show, Generic)

minus :: SmallInt -> SmallInt -> SmallInt
minus n k | n >= k = n - k
          | otherwise = 0

lose_life :: SmallInt -> Status -> Status
lose_life k Status {..} = Status {lives = (lives `minus` k), ..}

lose_fate :: SmallInt -> Status -> Status
lose_fate k Status {..} = Status {fate = (fate `minus` k), ..}

strength :: Status -> SmallInt
strength Status {..} = base_strength + more_strength

combat_strength :: Status -> SmallInt
combat_strength Status {..} = base_strength + more_strength + combat_bonus

lose_strength :: SmallInt -> Status -> Status
lose_strength k Status {..} = Status {more_strength = (more_strength `minus` k), ..}

craft :: Status -> SmallInt
craft Status {..} = base_craft + more_craft

lose_craft :: SmallInt -> Status -> Status
lose_craft k Status {..} = Status {more_craft = (more_craft `minus` k), ..}

data Fork = Strength | Craft
  deriving (Eq, Ord, Show, Generic)

data Board = Board SmallInt Fork Status Position -- The int is the amount of time left
  deriving (Eq, Ord, Show, Generic)

instance Renderable Board where
    render = putStrLn . show

-- The only choices in this model are when to use fate tokens, and
-- which dice to reroll
data Move = Proceed
          | Reroll SmallInt -- The index of the die to reroll
  deriving (Eq, Show)

available_moves :: Position -> [Move]
available_moves SPortalOfPower = [Proceed]
available_moves (PortalOfPower _ _) = [Proceed, Reroll 0]
available_moves PlainOfPeril = [Proceed]
available_moves SMine = [Proceed]
available_moves (Mine _ _) = [Proceed, Reroll 0]
available_moves SVampire = [Proceed]
available_moves (Vampire _) = [Proceed, Reroll 0]
available_moves SPitFiends = [Proceed]
available_moves (PitFiends _) = [Proceed, Reroll 0]
available_moves (FightPitFiends _ Start) = [Proceed]
available_moves (FightPitFiends _ (Fight _ _)) = [Proceed, Reroll 0]
available_moves SCrypt = [Proceed]
available_moves (Crypt _ _) = [Proceed, Reroll 0]
available_moves SDiceWithDeath = [Proceed]
available_moves (DiceWithDeath _ _ _ _) = [Proceed, Reroll 0, Reroll 1]
available_moves (DiceWithDeathA _ _ _) = [Proceed, Reroll 0]
available_moves (DiceWithDeathB _ _ _) = [Proceed, Reroll 0]
available_moves SWerewolf = [Proceed]
available_moves (Werewolf _ _) = [Proceed, Reroll 0]
available_moves (SFightWerewolf _) = [Proceed]
available_moves (FightWerewolf _ _ _) = [Proceed, Reroll 0]
available_moves ValleyOfFire = []

d6 :: (Fractional p) => Probabilities p SmallInt
d6 = Probabilities [(p, 1), (p, 2), (p, 3), (p, 4), (p, 5), (p, 6)] where p = 1.0/6

roll_d3 :: (Fractional p) => Probabilities p SmallInt
roll_d3 = Probabilities [(p, 1), (p, 2), (p, 3)] where p = 1.0/3

-- Helper implementing symmetry collapse for situations where two dice
-- were rolled.
two_dice :: (Die -> Die -> a) -> Die -> Die -> a
two_dice f d_1 d_2 = f (d_1 `max` d_2) (d_1 `min` d_2)

-- Helper implementing symmetry collapse for situations where three dice
-- were rolled and we know one would always want to reroll the highest,
-- and the only thing that matters about the others is the sum.
three_dice :: (Die -> SmallInt -> a) -> Die -> Die -> Die -> a
three_dice f d_1 d_2 d_3 = f (maximum dice) (sum dice - maximum dice) where
    dice = [d_1, d_2, d_3]

do_move :: (Fractional p) => Move -> Board -> Probabilities p Board
do_move Proceed (Board n f s SPortalOfPower) = do
  d1 <- d6
  d2 <- d6
  return $ Board n f s $ two_dice PortalOfPower d1 d2
do_move Proceed (Board n f@Strength s (PortalOfPower d1 d2))
    | strength s >= d1 + d2 = return $ Board (n-1) f s PlainOfPeril
    | otherwise = return $ Board (n-1) f (lose_strength 1 s) SPortalOfPower
do_move Proceed (Board n f@Craft s (PortalOfPower d1 d2))
    | craft s >= d1 + d2 = return $ Board (n-1) f s PlainOfPeril
    | otherwise = return $ Board (n-1) f (lose_craft 1 s) SPortalOfPower
do_move (Reroll 0) (Board n f s (PortalOfPower _ d2)) = do
  new_d <- d6
  do_move Proceed (Board n f (lose_fate 1 s) (two_dice PortalOfPower new_d d2))
-- Hack timing: entry to Mine/Crypt doesn't cost a turn, but exit
-- does.  Why? To make sure that the time-major ordering on games
-- decreases with every move.
do_move Proceed (Board n f@Strength s PlainOfPeril) = return $ Board n f s SCrypt
do_move Proceed (Board n f@Craft s PlainOfPeril) = return $ Board n f s SMine
do_move Proceed (Board n f s SMine) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  return $ Board n f s $ three_dice Mine d1 d2 d3
do_move Proceed (Board n f s (Mine d1 d2_d3))
    -- Timing hack: Entering the Mine doesn't cost a turn, but exiting does.
    | craft s >= d1 + d2_d3 = return $ Board (n `minus` 2) f s SVampire
    | craft s + 1 == d1 + d2_d3 = return $ Board (n-1) f s SMine
    | craft s + 2 == d1 + d2_d3 = return $ Board (n-1) f s SPortalOfPower
    | craft s + 3 == d1 + d2_d3 = return $ Board (n-1) f s SPortalOfPower
    | otherwise = return $ Board (n `minus` 2) f s SPortalOfPower -- TODO Model the outside
do_move (Reroll 0) (Board n f s (Mine _ d2_d3)) = do
  new_d <- d6  -- May no longer be the maximum, but doesn't matter
  do_move Proceed (Board n f (lose_fate 1 s) (Mine new_d d2_d3))
do_move Proceed (Board n f s SVampire) = do
  d1 <- roll_d3
  return $ Board n f s $ Vampire d1
do_move Proceed (Board n f s (Vampire drain)) = do
  return $ Board (n-1) f (lose_life drain s) SPitFiends
do_move (Reroll 0) (Board n f s (Vampire _)) = do
  new_d <- roll_d3
  do_move Proceed $ (Board n f s (Vampire new_d))
do_move Proceed (Board n f s SPitFiends) = do
  d1 <- d6
  return $ Board n f s (PitFiends d1)
do_move Proceed (Board n f s (PitFiends d1)) =
    return $ Board n f s (FightPitFiends d1 Start)
do_move (Reroll 0) (Board n f s (PitFiends _)) = do
  new_d <- d6
  do_move Proceed $ Board n f (lose_fate 1 s) $ PitFiends new_d
do_move Proceed (Board n f s (FightPitFiends 0 Start)) =
    return $ Board (n-1) f s ValleyOfFire
do_move Proceed (Board n f s (FightPitFiends count Start)) = do
  d1 <- d6
  d2 <- d6
  return $ Board n f s $ FightPitFiends count $ Fight d1 d2
do_move Proceed (Board n f s (FightPitFiends count (Fight d1 d2)))
    | combat_strength s + d1 > 4 + d2  = return $ Board n f s (FightPitFiends (count - 1) Start)
    | combat_strength s + d1 == 4 + d2 = return $ Board (n-1) f s (FightPitFiends count Start)
    | otherwise = return $ Board (n-1) f (lose_life 1 s) (FightPitFiends count Start)
do_move (Reroll 0) (Board n f s (FightPitFiends count (Fight _ d2))) = do
  new_d <- d6
  do_move Proceed $ Board n f (lose_fate 1 s) $ FightPitFiends count $ Fight new_d d2
do_move Proceed (Board n f s SCrypt) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  return $ Board n f s $ three_dice Crypt d1 d2 d3
do_move Proceed (Board n f s (Crypt d1 d2_d3))
    -- Timing hack: Entering the Crypt doesn't cost a turn, but exiting does.
    | strength s >= d1 + d2_d3 = return $ Board (n `minus` 2) f s SDiceWithDeath
    | strength s + 1 == d1 + d2_d3 = return $ Board (n-1) f s SCrypt
    | strength s + 2 == d1 + d2_d3 = return $ Board (n-1) f s SPortalOfPower
    | strength s + 3 == d1 + d2_d3 = return $ Board (n-1) f s SPortalOfPower
    | otherwise = return $ Board (n `minus` 2) f s SPortalOfPower -- TODO Model the outside
do_move (Reroll 0) (Board n f s (Crypt _ d2_d3)) = do
  new_d <- d6  -- May no longer be the maximum, but doesn't matter
  do_move Proceed (Board n f (lose_fate 1 s) (Crypt new_d d2_d3))
do_move Proceed (Board n f s SDiceWithDeath) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  d4 <- d6
  return $ Board n f s $ two_dice (two_dice DiceWithDeath d1 d2) d3 d4
do_move Proceed (Board n f s (DiceWithDeath d1 d2 d3 d4))
    | d1 + d2 >  d3 + d4 = return $ Board (n-1) f s SWerewolf
    | d1 + d2 == d3 + d4 = return $ Board (n-1) f s SDiceWithDeath
    | otherwise = return $ Board (n-1) f (lose_life 1 s) SDiceWithDeath
do_move (Reroll 0) (Board n f s (DiceWithDeath d1 _ d3 d4)) = do
  new_d <- d6
  return $ Board n f (lose_fate 1 s) $ two_dice (DiceWithDeathA $ d1 + new_d) d3 d4
do_move (Reroll 1) (Board n f s (DiceWithDeath d1 d2 _ d4)) = do
  new_d <- d6
  return $ Board n f (lose_fate 1 s) $ (two_dice DiceWithDeathB d1 d2) $ new_d + d4
do_move Proceed (Board n f s (DiceWithDeathA me d3 d4)) =
    do_move Proceed $ Board n f s $ DiceWithDeath me 0 d3 d4
do_move (Reroll 0) (Board n f s (DiceWithDeathA me _ d4)) = do
  new_d <- d6
  do_move Proceed $ Board n f (lose_fate 1 s) $ DiceWithDeath me 0 new_d d4
do_move Proceed (Board n f s (DiceWithDeathB d1 d2 death)) =
    do_move Proceed $ Board n f s $ DiceWithDeath d1 d2 death 0
do_move (Reroll 0) (Board n f s (DiceWithDeathB d1 _ death)) = do
  new_d <- d6
  do_move Proceed $ Board n f (lose_fate 1 s) $ DiceWithDeath d1 new_d death 0
do_move Proceed (Board n f s SWerewolf) = do
  d1 <- d6
  d2 <- d6
  return $ Board n f s $ two_dice Werewolf d1 d2
do_move Proceed (Board n f s (Werewolf d1 d2)) =
    return $ Board n f s $ SFightWerewolf $ d1 + d2
do_move (Reroll 0) (Board n f s (Werewolf _ d2)) = do
  new_d <- d6
  do_move Proceed $ Board n f (lose_fate 1 s) $ Werewolf new_d d2
do_move Proceed (Board n f s (SFightWerewolf str)) = do
  d1 <- d6
  d2 <- d6
  return $ Board n f s $ FightWerewolf str d1 d2
do_move Proceed (Board n f s (FightWerewolf str d1 d2))
    | combat_strength s + d1 > str + d2  = return $ Board (n-1) f s ValleyOfFire
    | combat_strength s + d1 == str + d2 = return $ Board (n-1) f s (SFightWerewolf str)
    | otherwise = return $ Board (n-1) f (lose_life 1 s) (SFightWerewolf str)
do_move (Reroll 0) (Board n f s (FightWerewolf str _ d2)) = do
  new_d <- d6
  do_move Proceed $ Board n f (lose_fate 1 s) $ FightWerewolf str new_d d2
do_move mv brd = error $ "Move " ++ show mv ++ " is not legal from board " ++ show brd

instance RGame Board Move where
    type Player Board = Solitaire
    moves (Board _ _ Status{..} p) = filter fated $ available_moves p where
                          fated (Reroll _) = fate > 0
                          fated _ = True
    r_move = do_move
    valid _ = const True -- TODO: Actually validate the moves
    payoff (Board 0 _ _ _) Self = Just 0
    payoff (Board 1 _ _ SWerewolf) Self = Just 0
    payoff (Board 1 _ _ SPitFiends) Self = Just 0
    payoff (Board time _ _ SDiceWithDeath) Self | time <= 2 = Just 0
    payoff (Board time _ _ SVampire) Self | time <= 2 = Just 0
    -- Hack timing: entry to Mine/Crypt doesn't cost a turn, but exit does.
    payoff (Board time _ _ SCrypt) Self | time <= 4 = Just 0
    payoff (Board time _ _ SMine) Self | time <= 4 = Just 0
    payoff (Board time _ _ PlainOfPeril) Self | time <= 4 = Just 0
    payoff (Board time _ _ SPortalOfPower) Self | time <= 5 = Just 0
    payoff (Board _ _ Status{lives=0} _) Self = Just 0
    payoff (Board _ _ _ ValleyOfFire) Self = Just 1
    payoff _ Self = Nothing
    current = const Self
