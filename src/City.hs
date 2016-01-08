{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module City where

import Control.Lens
import Control.Monad.State
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe

data Unit = Settler | Militia
    deriving (Eq, Ord, Show)

unit_cost :: Unit -> Int
unit_cost Settler = 40
unit_cost Militia = 10

data Resource = Food | Shield | Trade | Gold | Bulb
    deriving (Eq, Ord, Show)

newtype Stack a = Stack { _stack_things :: (M.Map a Int) }
    deriving (Eq, Ord, Show)

makeLenses ''Stack

type instance Index (Stack a) = a
type instance IxValue (Stack a) = Int
instance (Ord a) => Ixed (Stack a) where
    ix = ixAt
instance (Ord a) => At (Stack a) where
    at index = stack_things . at index

instance (Ord a) => Monoid (Stack a) where
    mempty = Stack M.empty
    (Stack m1) `mappend` (Stack m2) = Stack $ M.unionWith (+) m1 m2

one :: a -> Stack a
one x = Stack $ M.singleton x 1

two :: a -> Stack a
two x = Stack $ M.singleton x 2

type Resources = Stack Resource

data Tile = Tile { production :: Resources }
    deriving (Eq, Ord, Show)

forest, grassland :: Tile
forest = Tile $ one Food `mappend` two Shield
grassland = Tile $ two Food

data City = City { _center :: Tile
                 , _available :: [Tile]
                 , _pop :: Int
                 , _storage :: Resources
                 }
    deriving (Eq, Ord, Show)

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
-- - In what order is population decline from building a settler
--   checked vs growth from having lots of food?  Can building a
--   settler lead to a temporarily overfull food box?
grow :: Resources -> Unit -> City -> (Maybe Unit, City)
grow increment order city = runState act city where
    act :: State City (Maybe Unit)
    act = do
      storage <>= increment
      feed_populace
      starve_check
      grow_check
      maybe_produce

    feed_populace = do
      population <- use pop
      stored Food -= (population * 2)

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
        built_settler_check
        return $ Just order
      else return Nothing

    built_settler_check = if order == Settler then do
      pop -= 1 -- Civ 1 rules
    else return ()

-- May need to think about specialists' auto-happiness
production_orders :: City -> S.Set Resources
production_orders City{..} = S.map (mappend $ production _center) options where
    options = chooseMonoid _pop $ map production _available

-- Choose exactly k elements from the list of options; combine them;
-- and eliminate duplicates
chooseMonoid :: (Eq m, Ord m, Monoid m) => Int -> [m] -> S.Set m
chooseMonoid 0 _ = S.singleton mempty
chooseMonoid _ [] = S.empty
chooseMonoid k (opt:opts) = take `S.union` leave where
    take = S.map (mappend opt) $ chooseMonoid (k-1) opts
    leave = chooseMonoid k opts

-- chooseMonoid 3 $ [[], [1], [2,3] ,[4]]
-- fromList [[1,2,3],[1,2,3,4],[1,4],[2,3,4]]

-- production_orders $ City forest [forest] 1 mempty
-- fromList [Stack {_stack_things = fromList [(Food,2),(Shield,4)]}]

-- production_orders $ City forest [forest, forest, grassland] 2 mempty
-- fromList [Stack {_stack_things = fromList [(Food,3),(Shield,6)]},Stack {_stack_things = fromList [(Food,4),(Shield,4)]}]

bind' :: (Ord b) => S.Set a -> (a -> S.Set b) -> S.Set b
bind' opts f = S.unions $ map f $ S.toList opts

return' :: a -> S.Set a
return' = S.singleton

possible_turns :: City -> S.Set (Maybe Unit, City)
possible_turns c = production_orders c `bind'` \prod ->
  S.fromList [Settler, Militia] `bind'` \unit ->
  return' $ grow prod unit c

-- possible_turns $ City forest [forest, forest, grassland] 2 mempty
-- - two possibilities, no construction, either 6 shields and lose a
--   person or 4 shields and don't

-- Compute the results from the best production sequence of k turns
-- from a given starting city.
best_score :: Int -> City -> Int
best_score 0 = const 0
best_score k = go where
  go city = S.foldr' max 0 results
      where
        results = possible_turns city `bind'` (\(unit, city') ->
          return' $ score unit + answer city')
  score Nothing = 0
  score (Just Settler) = 1
  score (Just Militia) = 0
  answer = memoize $ best_score (k-1)

memoize :: (Ord a) => (a -> r) -> a -> r
memoize f = unsafePerformIO (do
  cacheRef <- newIORef M.empty
  return $ \x -> unsafePerformIO (do
                   cache <- readIORef cacheRef
                   case M.lookup x cache of
                     (Just v) -> return v
                     Nothing -> do
                       let v = f x
                       modifyIORef' cacheRef (M.insert x v)
                       return v))

-- Preliminary results
-- best_score 100 $ City forest [forest, forest, grassland] 1 mempty
-- 7
-- (10.94 secs, 5,471,597,824 bytes)
