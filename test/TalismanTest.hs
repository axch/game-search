{-# LANGUAGE RecordWildCards #-}

module TalismanTest where

import Control.Monad
import Test.QuickCheck

import GameSearch.Games.Talisman hiding (two_dice)

instance Arbitrary Status where

    arbitrary = do
      lives <- choose (0, 10)
      fate <- choose (0, 7)
      base_strength <- choose (0, 10)
      more_strength <- choose (0, 8)
      combat_bonus <- choose (0, 4)
      base_craft <- choose (0, 10)
      more_craft <- choose (0, 8)
      return Status{..}

    shrink = genericShrink


die :: Gen Die
die = choose (1, 6)

two_dice :: Gen SmallInt
two_dice = choose (2, 12)

instance Arbitrary Position where

    arbitrary = oneof
      [ return ValleyOfFire
      , liftM3 FightWerewolf two_dice die die
      , liftM SFightWerewolf two_dice
      , liftM2 Werewolf die die
      , return SWerewolf
      , liftM3 DiceWithDeathA two_dice die die
      , liftM3 DiceWithDeathB die die two_dice
      , liftM4 DiceWithDeath die die die die
      , return SDiceWithDeath
      , liftM2 Crypt die two_dice
      , return SCrypt
      , liftM3 FightPitFiends die die die
      , liftM SFightPitFiends die
      , liftM PitFiends die
      , return SPitFiends
      , liftM Vampire $ choose (1, 3)
      , return SVampire
      , liftM2 Mine die two_dice
      , return SMine
      , return PlainOfPeril
      , liftM2 PortalOfPower die die
      , return SPortalOfPower
      ]

    shrink = genericShrink

instance Arbitrary Fork where
    arbitrary = oneof [return Strength, return Craft]
    shrink = genericShrink

instance Arbitrary Board where
    arbitrary = liftM4 Board (choose (0, 20)) arbitrary arbitrary arbitrary
    shrink = genericShrink
