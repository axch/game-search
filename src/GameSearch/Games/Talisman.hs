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

-- So far, this is a reasonable model of the Crypt-Death-Werewolf fork.

-- TODO expand the model with:
-- + The craft attribute
-- + The Mine-Vampire-Pits fork
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
-- - Since I know which die is best to reroll in any situation, I can
--   reduce the number of situations that need to be considered by
--   collapsing out die order, and only allowing one "Reroll" move.
-- - I can reuse the move cache across multiple top-level position
--   evaluations in the main driver.

import Data.Word (Word8)

import GameSearch.Types

type SmallInt = Word8
type Die = SmallInt

-- Positions prefixed with "S" are starting points, where one has not
-- yet rolled any dice for the space.
data Position = ValleyOfFire
              | SWerewolf
              | Werewolf Die Die
              | SFightWerewolf SmallInt
              | FightWerewolf SmallInt Die Die
              | SDiceWithDeath
              | DiceWithDeath Die Die Die Die
              | DiceWithDeathA Die Die Die Die -- Rerolled one of mine first
              | DiceWithDeathB Die Die Die Die -- Rerolled one of Death's first
              | SCrypt
              | Crypt Die Die Die
              | SPitFiends
              | PitFiends Die
              | SFightPitFiends SmallInt
              | FightPitFiends SmallInt Die Die
              | SVampire
              | Vampire Die
              | SMine
              | Mine Die Die Die
              | PlainOfPeril
              | SPortalOfPower
              | PortalOfPower Die Die
  deriving (Eq, Ord, Show)

data Status = Status
    { lives :: SmallInt
    , fate :: SmallInt
    , base_strength :: SmallInt
    , more_strength :: SmallInt
    , combat_bonus :: SmallInt
    , base_craft :: SmallInt
    , more_craft :: SmallInt
    }
  deriving (Eq, Ord, Show)

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

data Board = Board SmallInt Status Position -- The int is the amount of time left
  deriving (Eq, Ord, Show)

instance Renderable Board where
    render = putStrLn . show

-- The only choices in this model are when to use fate tokens, and
-- which dice to reroll
data Move = Proceed
          | Reroll SmallInt -- The index of the die to reroll
  deriving (Eq, Show)

available_moves :: Position -> [Move]
available_moves SPortalOfPower = [Proceed]
available_moves (PortalOfPower _ _) = [Proceed, Reroll 0, Reroll 1]
available_moves PlainOfPeril = [Proceed]
available_moves SMine = [Proceed]
available_moves (Mine _ _ _) = [Proceed, Reroll 0, Reroll 1, Reroll 2]
available_moves SVampire = [Proceed]
available_moves (Vampire _) = [Proceed, Reroll 0]
available_moves SPitFiends = [Proceed]
available_moves (PitFiends _) = [Proceed, Reroll 0]
available_moves (SFightPitFiends _) = [Proceed]
available_moves (FightPitFiends _ _ _) = [Proceed, Reroll 0]
available_moves SCrypt = [Proceed]
available_moves (Crypt _ _ _) = [Proceed, Reroll 0, Reroll 1, Reroll 2]
available_moves SDiceWithDeath = [Proceed]
available_moves (DiceWithDeath _ _ _ _) = [Proceed, Reroll 0, Reroll 1, Reroll 2, Reroll 3]
available_moves (DiceWithDeathA _ _ _ _) = [Proceed, Reroll 0, Reroll 1]
available_moves (DiceWithDeathB _ _ _ _) = [Proceed, Reroll 0, Reroll 1]
available_moves SWerewolf = [Proceed]
available_moves (Werewolf _ _) = [Proceed, Reroll 0, Reroll 1]
available_moves (SFightWerewolf _) = [Proceed]
available_moves (FightWerewolf _ _ _) = [Proceed, Reroll 0]
available_moves ValleyOfFire = []

d6 :: (Fractional p) => Probabilities p SmallInt
d6 = Probabilities [(p, 1), (p, 2), (p, 3), (p, 4), (p, 5), (p, 6)] where p = 1.0/6

d3 :: (Fractional p) => Probabilities p SmallInt
d3 = Probabilities [(p, 1), (p, 2), (p, 3)] where p = 1.0/3

-- TODO: Define the dice lenses and rewrite all the Reroll cases to
-- pick a lens and reroll that die.
do_move :: (Fractional p) => Move -> Board -> Probabilities p Board
do_move Proceed (Board n s SPortalOfPower) = do
  d1 <- d6
  d2 <- d6
  return $ Board n s $ PortalOfPower d1 d2
do_move Proceed (Board n s (PortalOfPower d1 d2))
    | strength s >= d1 + d2 = return $ Board (n-1) s PlainOfPeril
    | otherwise = return $ Board (n-1) (lose_strength 1 s) SPortalOfPower
do_move (Reroll 0) (Board n s (PortalOfPower _ d2)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (PortalOfPower new_d d2))
do_move (Reroll 1) (Board n s (PortalOfPower d1 _)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (PortalOfPower d1 new_d))
do_move Proceed (Board n s PlainOfPeril) = return $ Board (n-1) s SCrypt
do_move Proceed (Board n s SMine) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  return $ Board n s $ Mine d1 d2 d3
do_move Proceed (Board n s (Mine d1 d2 d3))
    | craft s >= d1 + d2 + d3 = return $ Board (n-1) s SVampire
    | craft s + 1 == d1 + d2 + d3 = return $ Board (n-1) s SMine
    | craft s + 2 == d1 + d2 + d3 = return $ Board n s SPortalOfPower -- Fencepost on counting steps
    | craft s + 3 == d1 + d2 + d3 = return $ Board n s SPortalOfPower
    | otherwise = return $ Board (n-1) s SPortalOfPower -- TODO Model the outside
do_move (Reroll 0) (Board n s (Mine _ d2 d3)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (Mine new_d d2 d3))
do_move (Reroll 1) (Board n s (Mine d1 _ d3)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (Mine d1 new_d d3))
do_move (Reroll 2) (Board n s (Mine d1 d2 _)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (Mine d1 d2 new_d))
do_move Proceed (Board n s SVampire) = do
  d1 <- d3
  return $ Board n s $ Vampire d1
do_move Proceed (Board n s (Vampire drain)) = do
  return $ Board (n-1) (lose_life drain s) SPitFiends
do_move (Reroll 0) (Board n s (Vampire _)) = do
  new_d <- d3
  do_move Proceed $ (Board n s (Vampire new_d))
do_move Proceed (Board n s SPitFiends) = do
  d1 <- d6
  return $ Board n s (PitFiends d1)
do_move Proceed (Board n s (PitFiends d1)) =
    return $ Board n s (SFightPitFiends d1)
do_move (Reroll 0) (Board n s (PitFiends _)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ PitFiends new_d
do_move Proceed (Board n s (SFightPitFiends 0)) =
    return $ Board (n-1) s ValleyOfFire
do_move Proceed (Board n s (SFightPitFiends count)) = do
  d1 <- d6
  d2 <- d6
  return $ Board n s $ FightPitFiends count d1 d2
do_move Proceed (Board n s (FightPitFiends count d1 d2))
    | combat_strength s + d1 > 4 + d2  = return $ Board n s (SFightPitFiends $ count - 1)
    | combat_strength s + d1 == 4 + d2 = return $ Board (n-1) s (SFightPitFiends count)
    | otherwise = return $ Board (n-1) (lose_life 1 s) (SFightPitFiends count)
do_move (Reroll 0) (Board n s (FightPitFiends count _ d2)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ FightPitFiends count new_d d2
do_move Proceed (Board n s SCrypt) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  return $ Board n s $ Crypt d1 d2 d3
do_move Proceed (Board n s (Crypt d1 d2 d3))
    | strength s >= d1 + d2 + d3 = return $ Board (n-1) s SDiceWithDeath
    | strength s + 1 == d1 + d2 + d3 = return $ Board (n-1) s SCrypt
    | strength s + 2 == d1 + d2 + d3 = return $ Board n s SPortalOfPower -- Fencepost on counting steps
    | strength s + 3 == d1 + d2 + d3 = return $ Board n s SPortalOfPower
    | otherwise = return $ Board (n-1) s SPortalOfPower -- TODO Model the outside
do_move (Reroll 0) (Board n s (Crypt _ d2 d3)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (Crypt new_d d2 d3))
do_move (Reroll 1) (Board n s (Crypt d1 _ d3)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (Crypt d1 new_d d3))
do_move (Reroll 2) (Board n s (Crypt d1 d2 _)) = do
  new_d <- d6
  do_move Proceed (Board n (lose_fate 1 s) (Crypt d1 d2 new_d))
do_move Proceed (Board n s SDiceWithDeath) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  d4 <- d6
  return $ Board n s $ DiceWithDeath d1 d2 d3 d4
do_move Proceed (Board n s (DiceWithDeath d1 d2 d3 d4))
    | d1 + d2 >  d3 + d4 = return $ Board (n-1) s SWerewolf
    | d1 + d2 == d3 + d4 = return $ Board (n-1) s SDiceWithDeath
    | otherwise = return $ Board (n-1) (lose_life 1 s) SDiceWithDeath
do_move (Reroll 0) (Board n s (DiceWithDeath _ d2 d3 d4)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathA new_d d2 d3 d4
do_move (Reroll 1) (Board n s (DiceWithDeath d1 _ d3 d4)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathA d1 new_d d3 d4
do_move (Reroll 2) (Board n s (DiceWithDeath d1 d2 _ d4)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathB d1 d2 new_d d4
do_move (Reroll 3) (Board n s (DiceWithDeath d1 d2 d3 _)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathB d1 d2 d3 new_d
do_move Proceed (Board n s (DiceWithDeathA d1 d2 d3 d4)) =
    do_move Proceed $ Board n s $ DiceWithDeath d1 d2 d3 d4
do_move (Reroll 0) (Board n s (DiceWithDeathA d1 d2 _ d4)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ DiceWithDeath d1 d2 new_d d4
do_move (Reroll 1) (Board n s (DiceWithDeathA d1 d2 d3 _)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ DiceWithDeath d1 d2 d3 new_d
do_move Proceed (Board n s (DiceWithDeathB d1 d2 d3 d4)) =
    do_move Proceed $ Board n s $ DiceWithDeath d1 d2 d3 d4
do_move (Reroll 0) (Board n s (DiceWithDeathB _ d2 d3 d4)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ DiceWithDeath new_d d2 d3 d4
do_move (Reroll 1) (Board n s (DiceWithDeathB d1 _ d3 d4)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ DiceWithDeath d1 new_d d3 d4
do_move Proceed (Board n s SWerewolf) = do
  d1 <- d6
  d2 <- d6
  return $ Board n s $ Werewolf d1 d2
do_move Proceed (Board n s (Werewolf d1 d2)) =
    return $ Board n s $ SFightWerewolf $ d1 + d2
do_move (Reroll 0) (Board n s (Werewolf _ d2)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ Werewolf new_d d2
do_move (Reroll 1) (Board n s (Werewolf d1 _)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ Werewolf d1 new_d
do_move Proceed (Board n s (SFightWerewolf str)) = do
  d1 <- d6
  d2 <- d6
  return $ Board n s $ FightWerewolf str d1 d2
do_move Proceed (Board n s (FightWerewolf str d1 d2))
    | combat_strength s + d1 > str + d2  = return $ Board (n-1) s ValleyOfFire
    | combat_strength s + d1 == str + d2 = return $ Board (n-1) s (SFightWerewolf str)
    | otherwise = return $ Board (n-1) (lose_life 1 s) (SFightWerewolf str)
do_move (Reroll 0) (Board n s (FightWerewolf str _ d2)) = do
  new_d <- d6
  do_move Proceed $ Board n (lose_fate 1 s) $ FightWerewolf str new_d d2
do_move mv brd = error $ "Move " ++ show mv ++ " is not legal from board " ++ show brd

instance RGame Board Move where
    type Player Board = Solitaire
    moves (Board _ Status{..} p) | fate == 0 = [Proceed]
                                 | otherwise = available_moves p
    r_move = do_move
    valid _ = const True -- TODO: Actually validate the moves
    payoff (Board 0 _ _) Self = Just 0
    payoff (Board _ Status{lives=0} _) Self = Just 0
    payoff (Board _ _ ValleyOfFire) Self = Just 1
    payoff _ Self = Nothing
    current = const Self
