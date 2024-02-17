-- Copyright 2020 Alexey Radul

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
{-# LANGUAGE RecordWildCards #-}

module GameSearch.Games.AxisAndAlliesCombat where

-- Combat in the game Axis and Allies.

import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

data Target = Air | Land | Sea
  deriving (Eq, Ord, Show, Generic)

data Unit = Unit
    { body_type :: Target
    , no_air_attack :: Bool
    , cost :: Int
    , movement :: Int
    , attack :: Int
    , defense :: Int
    }
  deriving (Eq, Ord, Show, Generic)

data Force = Force (M.Map Unit Int)
  deriving (Eq, Ord, Show, Generic)

data Clock = AAGunsFire
           | RemoveAirCasualties Int
           -- TODO: Model battleship free shot
           | AttackSubmarinesFire
           | RemoveSubmarineCasualties Int
           | AttackOthersFire
           | DefenderSelectCasualties Int
           | DefenderFire
           -- The first int is the number of attacker's casualties
           -- inflicted by submarines, the second by other units.
           | RemoveCasualties Int Int
           | AttackerSubmarinesMayWithdraw
           | DefenderSubmarinesMayWithdraw
           | AttackerMayWithdraw
           | Over  -- If both forces remain, attacker has withdrawn
  deriving (Eq, Ord, Show, Generic)

data Position = Position
    { step :: Clock
    , attacker :: Force
    , attacker_withdrawn_subs :: Int
    , defender :: Force
    , defender_withdrawn_subs :: Int
    , casualties :: Force
    }
  deriving (Eq, Ord, Show, Generic)

data Move = Proceed
          | Remove Unit
          | Withdraw
  deriving (Eq, Ord, Show, Generic)

targetable :: (Unit -> Bool) -> Force -> [Move]
targetable target (Force units) = map Remove $ filter target $ M.keys units

-- Casualty removal encoding is to remove one lost unit at a time, to
-- avoid materializing the set of all possible casualty removals.  In
-- the real game, it's only rarely non-obvious what unit to remove as
-- a casualty, so I expect to model this choice with heuristics most
-- of the time.
available_moves :: Position -> [Move]
available_moves Position{ step = AAGunsFire } = [Proceed]
available_moves Position{ step = (RemoveAirCasualties _n), .. } =
    targetable target attacker where
        target Unit{ body_type = Air } = True
        target _ = False
available_moves Position{ step = AttackSubmarinesFire } = [Proceed]
available_moves Position{ step = (RemoveSubmarineCasualties _n), .. } =
    targetable target defender where
        target Unit{ body_type = Air } = False
        target _ = True
available_moves Position{ step = AttackOthersFire } = [Proceed]
available_moves Position{ step = (DefenderSelectCasualties _n), .. } =
    targetable (const True) defender where
available_moves Position{ step = DefenderFire } = [Proceed]
available_moves Position{ step = (RemoveCasualties _n1 _n2), .. } =
    targetable (const True) attacker where
available_moves Position{ step = AttackerSubmarinesMayWithdraw } = [Proceed, Withdraw]
available_moves Position{ step = DefenderSubmarinesMayWithdraw } = [Proceed, Withdraw]
available_moves Position{ step = AttackerMayWithdraw } = [Proceed, Withdraw]
available_moves Position{ step = Over } = []
