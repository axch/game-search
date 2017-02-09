{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Talisman where

import Types

type Die = Int

data Space = SValleyOfFire
           | SWerewolf
           | SFightWerewolf Int
           | SCrypt
           | SPlainOfPeril
           | SPortalOfPower

data Position = ValleyOfFire
              | Werewolf Die Die
              | FightWerewolf Int Die Die
              | SDiceWithDeath
              | DiceWithDeath Die Die Die Die
              | DiceWithDeathA Die Die Die Die -- Rerolled one of mine first
              | DiceWithDeathB Die Die Die Die -- Rerolled one of Death's first
              | Crypt Die Die Die
              | PlainOfPeril
              | PortalOfPower Die Die
  deriving (Eq, Ord, Show)

initial_position :: (Fractional p) => Space -> Probabilities p Position
initial_position SPortalOfPower = do
  d1 <- d6
  d2 <- d6
  return $ PortalOfPower d1 d2
initial_position SPlainOfPeril = return PlainOfPeril
initial_position SCrypt = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  return $ Crypt d1 d2 d3
initial_position SWerewolf = do
  d1 <- d6
  d2 <- d6
  return $ Werewolf d1 d2
initial_position (SFightWerewolf str) = do
  d1 <- d6
  d2 <- d6
  return $ FightWerewolf str d1 d2
initial_position SValleyOfFire = return ValleyOfFire

data Status = Status
    { lives :: Int
    , fate :: Int
    , base_strength :: Int
    , more_strength :: Int
    , combat_bonus :: Int
    }
  deriving (Eq, Ord, Show)

lose_life :: Int -> Status -> Status
lose_life k Status {..} = Status {lives = max 0 (lives - k), ..}

lose_fate :: Int -> Status -> Status
lose_fate k Status {..} = Status {fate = max 0 (fate - k), ..}

strength :: Status -> Int
strength Status {..} = base_strength + more_strength

combat_strength :: Status -> Int
combat_strength Status {..} = base_strength + more_strength + combat_bonus

lose_strength :: Int -> Status -> Status
lose_strength k Status {..} = Status {more_strength = max 0 (more_strength - k), ..}

data Board = Board Int Status Position -- The int is the amount of time left
  deriving (Eq, Ord, Show)

instance Renderable Board where
    render = putStrLn . show

move_to :: (Fractional p) => Space -> Board -> Probabilities p Board
move_to spc (Board n s _) = fmap (Board (n-1) s) $ initial_position spc

-- The only choices in this model are when to use fate tokens, and
-- which dice to reroll
data Move = Accept
          | Reroll Int -- The index of the die to reroll
  deriving Show

available_moves :: Position -> [Move]
available_moves (PortalOfPower _ _) = [Accept, Reroll 0, Reroll 1]
available_moves PlainOfPeril = [Accept]
available_moves (Crypt _ _ _) = [Accept, Reroll 0, Reroll 1, Reroll 2]
available_moves SDiceWithDeath = [Accept]
available_moves (DiceWithDeath _ _ _ _) = [Accept, Reroll 0, Reroll 1, Reroll 2, Reroll 3]
available_moves (DiceWithDeathA _ _ _ _) = [Accept, Reroll 0, Reroll 1]
available_moves (DiceWithDeathB _ _ _ _) = [Accept, Reroll 0, Reroll 1]
available_moves (Werewolf _ _) = [Accept, Reroll 0, Reroll 1]
available_moves (FightWerewolf _ _ _) = [Accept, Reroll 0]
available_moves ValleyOfFire = []


-- TODO: Model:
-- - The craft attribute
-- - Choosing whether to open the Portal with craft (as opposed to strength)
-- - The craft path
-- - Modeling the outside world, for variable results from the mine
-- - Modeling the gnome or dwarf ability

d6 :: (Fractional p) => Probabilities p Int
d6 = Probabilities [(p, 1), (p, 2), (p, 3), (p, 4), (p, 5), (p, 6)] where p = 1.0/6

-- TODO: Define the dice lenses and rewrite all the Reroll cases to
-- pick a lens and reroll that die.
-- TODO: Bug: All the dice rolling and resolution is part of the same
-- turn as moving a space.  I think the Crypt currently takes too many
-- turns to deal with.
do_move :: (Fractional p) => Move -> Board -> Probabilities p Board
do_move Accept b@(Board n s p@(PortalOfPower d1 d2))
    | strength s >= d1 + d2 = move_to SPlainOfPeril b
    | otherwise = move_to SPortalOfPower $ (Board n (lose_strength 1 s) p)
do_move (Reroll 0) (Board n s (PortalOfPower d1 d2)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (PortalOfPower new_d d2))
do_move (Reroll 1) (Board n s (PortalOfPower d1 d2)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (PortalOfPower d1 new_d))
do_move Accept b@(Board _ _ PlainOfPeril) = move_to SCrypt b
do_move Accept b@(Board n s (Crypt d1 d2 d3))
    | strength s >= d1 + d2 + d3 = return $ Board (n-1) s SDiceWithDeath
    | strength s + 1 == d1 + d2 + d3 = move_to SPlainOfPeril b
    | strength s + 2 == d1 + d2 + d3 = move_to SPortalOfPower b
    | strength s + 3 == d1 + d2 + d3 = move_to SPortalOfPower b
    | otherwise = move_to SPortalOfPower b -- TODO Model the outside
do_move (Reroll 0) (Board n s (Crypt d1 d2 d3)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (Crypt new_d d2 d3))
do_move (Reroll 1) (Board n s (Crypt d1 d2 d3)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (Crypt d1 new_d d3))
do_move (Reroll 2) (Board n s (Crypt d1 d2 d3)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (Crypt d1 d2 new_d))
do_move Accept b@(Board n s SDiceWithDeath) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  d4 <- d6
  return $ Board n s $ DiceWithDeath d1 d2 d3 d4
do_move Accept b@(Board n s p@(DiceWithDeath d1 d2 d3 d4))
    | d1 + d2 >  d3 + d4 = move_to SWerewolf b
    | d1 + d2 == d3 + d4 = return $ Board (n-1) s SDiceWithDeath
    | otherwise = return $ Board (n-1) (lose_life 1 s) SDiceWithDeath
do_move (Reroll 0) (Board n s (DiceWithDeath d1 d2 d3 d4)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathA new_d d2 d3 d4
do_move (Reroll 1) (Board n s (DiceWithDeath d1 d2 d3 d4)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathA d1 new_d d3 d4
do_move (Reroll 2) (Board n s (DiceWithDeath d1 d2 d3 d4)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathB d1 d2 new_d d4
do_move (Reroll 3) (Board n s (DiceWithDeath d1 d2 d3 d4)) = do
  new_d <- d6
  return $ Board n (lose_fate 1 s) $ DiceWithDeathB d1 d2 d3 new_d
do_move Accept (Board n s (DiceWithDeathA d1 d2 d3 d4)) =
    do_move Accept $ Board n s $ DiceWithDeath d1 d2 d3 d4
do_move (Reroll 0) (Board n s (DiceWithDeathA d1 d2 d3 d4)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ DiceWithDeath d1 d2 new_d d4
do_move (Reroll 1) (Board n s (DiceWithDeathA d1 d2 d3 d4)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ DiceWithDeath d1 d2 d3 new_d
do_move Accept (Board n s (DiceWithDeathB d1 d2 d3 d4)) =
    do_move Accept $ Board n s $ DiceWithDeath d1 d2 d3 d4
do_move (Reroll 0) (Board n s (DiceWithDeathB d1 d2 d3 d4)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ DiceWithDeath new_d d2 d3 d4
do_move (Reroll 1) (Board n s (DiceWithDeathB d1 d2 d3 d4)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ DiceWithDeath d1 new_d d3 d4
do_move Accept (Board n s (Werewolf d1 d2)) =
    fmap (Board n s) $ initial_position (SFightWerewolf $ d1 + d2)
do_move (Reroll 0) (Board n s (Werewolf d1 d2)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ Werewolf new_d d2
do_move (Reroll 1) (Board n s (Werewolf d1 d2)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ Werewolf d1 new_d
do_move Accept b@(Board n s p@(FightWerewolf str d1 d2))
    | combat_strength s + d1 > str + d2  = move_to SValleyOfFire b
    | combat_strength s + d1 == str + d2 = move_to (SFightWerewolf str) b
    | otherwise = move_to (SFightWerewolf str) $ Board n (lose_life 1 s) p
do_move (Reroll 0) (Board n s (FightWerewolf str d1 d2)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ FightWerewolf str new_d d2

instance RGame Board Move where
    moves (Board n Status{..} p) | fate == 0 = [Accept]
                                 | otherwise = available_moves p
    r_move = do_move
    valid m = const True -- TODO: Actually validate the moves
    start = undefined -- There are many possible start statuses
    payoff (Board 0 _ _) (Player 0) = Just 0
    payoff (Board _ Status{lives=0} _) (Player 0) = Just 0
    payoff (Board _ _ ValleyOfFire) (Player 0) = Just 1
    payoff _ (Player 0) = Nothing
    current = const (Player 0)
