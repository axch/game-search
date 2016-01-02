{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module City where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M

data Unit = Settler | Militia

unit_cost :: Unit -> Int
unit_cost Settler = 40
unit_cost Militia = 10

data Resource = Food | Shield | Trade | Gold | Bulb
    deriving (Eq, Ord)

newtype Stack a = Stack { _stack_things :: (M.Map a Int) }

makeLenses ''Stack

instance (Ord a) => Num (Stack a) where
    (Stack m1) + (Stack m2) = Stack $ M.unionWith (+) m1 m2
    negate (Stack m1) = Stack $ fmap negate m1
    abs (Stack m1) = Stack $ fmap abs m1
    -- No *, signum, or fromInteger, because it's really vector
    -- addition I want here.

type instance Index (Stack a) = a
type instance IxValue (Stack a) = Int
instance (Ord a) => Ixed (Stack a) where
    ix = ixAt
instance (Ord a) => At (Stack a) where
    at index = stack_things . at index

type Resources = Stack Resource

data Tile = Tile { production :: Resources }

data City = City { _center :: Tile
                 , _available :: [Tile]
                 , _pop :: Int
                 , _storage :: Resources
                 }

makeLenses ''City

stored :: Resource -> Lens City City Int Int
stored r = storage . at r . non 0

-- The amount of food necessary to grow one more person, for a given
-- number of people present.
food_capacity :: Int -> Int
food_capacity people = 10 * (people + 1) -- Civ 1 rules.

-- Mechanics of growth and production
-- Ignore output of gold or research for now
-- TODO: Things to find out:
-- - Do starvation or growth affect that turn's shield production?
grow :: Resources -> Unit -> City -> (Maybe Unit, City)
grow increment order city = runState act city where
    act :: State City (Maybe Unit)
    act = do
      storage += increment
      starve_check
      grow_check
      maybe_produce

    starve_check = do
      food <- use $ stored Food
      if food < 0 then do
         stored Food .= 0
         pop -= 1
      else return ()

    grow_check = do
      food <- use $ stored Food
      food_cap <- uses pop food_capacity
      if food >= food_cap then do
         stored Food .= 0 -- Ignoring granary improvement for now
         pop += 1
      else return ()

    maybe_produce = do
      shields <- use $ stored Shield
      if shields >= unit_cost order then do
         stored Shield .= 0
         return $ Just order
      else return Nothing
