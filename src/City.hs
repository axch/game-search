{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module City where

import Control.Lens
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import System.IO.Unsafe
import Text.Printf

import Rig hiding (one) -- Wanted to use Kmett's algebra package, but it wouldn't install
import qualified Rig (one)
import Searches

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

instance Bounded (Stack a) where
    minBound = Stack M.empty
    maxBound = undefined

instance Bounded [a] where
    minBound = []
    maxBound = undefined

one :: a -> Stack a
one x = Stack $ M.singleton x 1

two :: a -> Stack a
two x = Stack $ M.singleton x 2

class ShowShort a where
    show_short :: a -> String

instance ShowShort a => ShowShort (Stack a) where
    show_short (Stack m) = intercalate "," $ map item $ M.toList m where
        item (key, num) = show num ++ " " ++ show_short key

asList :: Stack a -> [a]
asList (Stack as) = concatMap groups $ M.toList as where
    groups (a, ct) = take ct $ repeat a

for :: (Monoid m) => (a -> m) -> Stack a -> m
for f as = foldl' mappend mempty $ map f $ asList as

type Resources = Stack Resource

data Tile = Tile { name :: String
                 , production :: Resources }
    deriving (Eq, Ord, Show)

instance ShowShort Tile where
    show_short = name

forest, grassland, grassland_sh :: Tile
forest = Tile "forest" $ one Food `mappend` two Shield
grassland = Tile "grassland" $ two Food
grassland_sh = Tile "grassland_sh" $ two Food `mappend` one Shield

data City = City { _center :: Tile
                 , _available :: [Tile]
                 , _pop :: Int
                 , _storage :: Resources
                 }
    deriving (Eq, Ord, Show)

makeLenses ''City

stored :: Resource -> Lens City City Int Int
stored r = storage . at r . non 0

instance ShowShort City where
    show_short c = printf "pop: %2d food: %2d shields: %2d"
                   (c ^. pop) (c ^. stored Food) (c ^. stored Shield)

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

production_orders' :: (Rig r) => (Tile -> r) -> City -> r
production_orders' prod City{..} = prod _center .*. options where
    options = chooseRig _pop $ map prod _available

-- May need to think about specialists' auto-happiness
production_orders :: City -> S.Set Resources
production_orders = asSet . production_orders' (SetOf . S.singleton . production)

production_orders_ann :: City -> M.Map Resources (MaxPlus (Stack Tile))
production_orders_ann = asMap . production_orders' prod where
    prod tile = Annotated $ M.singleton (production tile) (annotate tile)
    annotate = MaxPlus . one

-- production_orders $ City forest [forest] 1 mempty
-- fromList [Stack {_stack_things = fromList [(Food,2),(Shield,4)]}]

-- production_orders $ City forest [forest, forest, grassland] 2 mempty
-- fromList [Stack {_stack_things = fromList [(Food,3),(Shield,6)]},
--           Stack {_stack_things = fromList [(Food,4),(Shield,4)]}]

-- similarly production_orders_ann

bind' :: (Ord b) => S.Set a -> (a -> S.Set b) -> S.Set b
bind' opts f = S.unions $ map f $ S.toList opts

return' :: a -> S.Set a
return' = S.singleton

bind_ann :: (Ord b, Rig ann2) => M.Map a ann ->
            (a -> ann -> M.Map b ann2) -> M.Map b ann2
bind_ann opts f = foldl' (M.unionWith (.+.)) M.empty $ map (uncurry f)
                  $ M.toList opts

return_ann :: a -> ann -> M.Map a ann
return_ann = M.singleton

bind_ann_r :: (Rig r) => M.Map a ann -> (a -> ann -> r) -> r
bind_ann_r opts f = foldl' (.+.) zero $ map (uncurry f) $ M.toList opts

possible_turns :: City -> S.Set (Maybe Unit, City)
possible_turns c = production_orders c `bind'` \prod ->
  S.fromList [Settler, Militia] `bind'` \unit ->
  return' $ grow prod unit c

build_orders_ann :: M.Map Unit Unit
build_orders_ann = M.fromList [(Settler, Settler), (Militia, Militia)]

type OrderSet = ((MaxPlus (Stack Tile)), (MaxPlus (Stack Unit)))

possible_turns_ann :: City -> M.Map (Maybe Unit, City) OrderSet
possible_turns_ann c = production_orders_ann c `bind_ann` \prod tiles ->
  build_orders_ann `bind_ann` \unit unit' ->
  return_ann (grow prod unit c) $ (tiles, MaxPlus $ one unit')

-- possible_turns $ City forest [forest, forest, grassland] 2 mempty
-- - two possibilities, no construction, either 6 shields and lose a
--   person or 4 shields and don't

-- similarly possible_turns_ann

-- Compute the results from the best production sequence of k turns
-- from a given starting city.
best_score :: Int -> City -> Int
best_score 0 = const 0
best_score k = go where
  go City{_pop = 0} = 0
  go city = S.foldr' max 0 results
      where
        results = possible_turns city `bind'` (\(unit, city') ->
          return' $ score unit + answer city')
  score Nothing = 0
  score (Just Settler) = 1
  score (Just Militia) = 0
  answer = memoize $ best_score (k-1)

-- Preliminary results
-- best_score 100 $ City forest [forest, forest, grassland] 1 mempty
-- 7
-- (10.94 secs, 5,471,597,824 bytes)

best_score_ann :: Int -> City -> MaxPlusA (Sum Int) (MaxPlus [OrderSet])
best_score_ann 0 = const Rig.one
best_score_ann k = go where
  go City{_pop = 0} = Rig.one
  go city = possible_turns_ann city `bind_ann_r` (\(unit, city') orders ->
            (MaxPlusA (Sum $ score unit) $ MaxPlus [orders]) .*. answer city')
  score Nothing = 0
  score (Just Settler) = 1
  score (Just Militia) = 0
  answer = memoize $ best_score_ann (k-1)

-- Similarly best_score_ann

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

show_prod_seq :: City -> [OrderSet] -> String
show_prod_seq city [] = show_short city
show_prod_seq city ((MaxPlus tiles, MaxPlus (Stack units)):rest) = line ++ lines where
    line = show_short city ++ " + " ++ show_short tiles ++ built_str ++ "\n"
    (build_order, _) = M.findMin units
    (built, city') = grow (production `for` tiles) build_order city
    built_str = case built of
                  Nothing -> ""
                  (Just u) -> " -> " ++ show u
    lines = show_prod_seq city' rest

show_best_prod_seq :: Int -> City -> String
show_best_prod_seq k c = show score ++ "\n" ++ show_prod_seq c orders where
    (MaxPlusA (Sum score) (MaxPlus orders)) = best_score_ann k c

-- Preliminary results
-- putStrLn $ show_best_prod_seq 100 $ City forest [forest, forest, grassland] 1 mempty
-- 5 settlers, 12 seconds
-- putStrLn $ show_best_prod_seq 96 $ City grassland_sh [forest, forest, grassland_sh] 1 mempty
-- 7 settlers, 43 seconds
-- putStrLn $ show_best_prod_seq 90 $ City grassland_sh [forest, forest, grassland_sh, grassland_sh] 1 mempty
-- 7 settlers, 100 seconds
-- Still builds one militia; is it possible to get 7 faster?
