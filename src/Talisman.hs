{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Talisman where

import Types

type Die = Int

data Position = ValleyOfFire
              | SWerewolf
              | Werewolf Die Die
              | SFightWerewolf Int
              | FightWerewolf Int Die Die
              | SDiceWithDeath
              | DiceWithDeath Die Die Die Die
              | DiceWithDeathA Die Die Die Die -- Rerolled one of mine first
              | DiceWithDeathB Die Die Die Die -- Rerolled one of Death's first
              | SCrypt
              | Crypt Die Die Die
              | PlainOfPeril
              | SPortalOfPower
              | PortalOfPower Die Die
  deriving (Eq, Ord, Show)

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

-- The only choices in this model are when to use fate tokens, and
-- which dice to reroll
data Move = Accept
          | Reroll Int -- The index of the die to reroll
  deriving Show

available_moves :: Position -> [Move]
available_moves SPortalOfPower = [Accept]
available_moves (PortalOfPower _ _) = [Accept, Reroll 0, Reroll 1]
available_moves PlainOfPeril = [Accept]
available_moves SCrypt = [Accept]
available_moves (Crypt _ _ _) = [Accept, Reroll 0, Reroll 1, Reroll 2]
available_moves SDiceWithDeath = [Accept]
available_moves (DiceWithDeath _ _ _ _) = [Accept, Reroll 0, Reroll 1, Reroll 2, Reroll 3]
available_moves (DiceWithDeathA _ _ _ _) = [Accept, Reroll 0, Reroll 1]
available_moves (DiceWithDeathB _ _ _ _) = [Accept, Reroll 0, Reroll 1]
available_moves SWerewolf = [Accept]
available_moves (Werewolf _ _) = [Accept, Reroll 0, Reroll 1]
available_moves (SFightWerewolf _) = [Accept]
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
do_move :: (Fractional p) => Move -> Board -> Probabilities p Board
do_move Accept (Board n s SPortalOfPower) = do
  d1 <- d6
  d2 <- d6
  return $ Board n s $ PortalOfPower d1 d2
do_move Accept b@(Board n s (PortalOfPower d1 d2))
    | strength s >= d1 + d2 = return $ Board (n-1) s PlainOfPeril
    | otherwise = return $ Board (n-1) (lose_strength 1 s) SPortalOfPower
do_move (Reroll 0) (Board n s (PortalOfPower d1 d2)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (PortalOfPower new_d d2))
do_move (Reroll 1) (Board n s (PortalOfPower d1 d2)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (PortalOfPower d1 new_d))
do_move Accept (Board n s PlainOfPeril) = return $ Board (n-1) s SCrypt
do_move Accept (Board n s SCrypt) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  return $ Board n s $ Crypt d1 d2 d3
do_move Accept b@(Board n s (Crypt d1 d2 d3))
    | strength s >= d1 + d2 + d3 = return $ Board (n-1) s SDiceWithDeath
    | strength s + 1 == d1 + d2 + d3 = return $ Board (n-1) s SCrypt
    | strength s + 2 == d1 + d2 + d3 = return $ Board n s SPortalOfPower -- Fencepost on counting steps
    | strength s + 3 == d1 + d2 + d3 = return $ Board n s SPortalOfPower
    | otherwise = return $ Board (n-1) s SPortalOfPower -- TODO Model the outside
do_move (Reroll 0) (Board n s (Crypt d1 d2 d3)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (Crypt new_d d2 d3))
do_move (Reroll 1) (Board n s (Crypt d1 d2 d3)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (Crypt d1 new_d d3))
do_move (Reroll 2) (Board n s (Crypt d1 d2 d3)) = do
  new_d <- d6
  do_move Accept (Board n (lose_fate 1 s) (Crypt d1 d2 new_d))
do_move Accept (Board n s SDiceWithDeath) = do
  d1 <- d6
  d2 <- d6
  d3 <- d6
  d4 <- d6
  return $ Board n s $ DiceWithDeath d1 d2 d3 d4
do_move Accept (Board n s (DiceWithDeath d1 d2 d3 d4))
    | d1 + d2 >  d3 + d4 = return $ Board (n-1) s SWerewolf
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
do_move Accept (Board n s SWerewolf) = do
  d1 <- d6
  d2 <- d6
  return $ Board n s $ Werewolf d1 d2
do_move Accept (Board n s (Werewolf d1 d2)) =
    return $ Board n s $ SFightWerewolf $ d1 + d2
do_move (Reroll 0) (Board n s (Werewolf d1 d2)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ Werewolf new_d d2
do_move (Reroll 1) (Board n s (Werewolf d1 d2)) = do
  new_d <- d6
  do_move Accept $ Board n (lose_fate 1 s) $ Werewolf d1 new_d
do_move Accept (Board n s (SFightWerewolf str)) = do
  d1 <- d6
  d2 <- d6
  return $ Board n s $ FightWerewolf str d1 d2
do_move Accept b@(Board n s (FightWerewolf str d1 d2))
    | combat_strength s + d1 > str + d2  = return $ Board (n-1) s ValleyOfFire
    | combat_strength s + d1 == str + d2 = return $ Board (n-1) s (SFightWerewolf str)
    | otherwise = return $ Board (n-1) (lose_life 1 s) (SFightWerewolf str)
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
