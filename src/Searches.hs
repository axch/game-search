module Searches where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Rig

-- Choose exactly k elements from the list of options.  Combine
-- elements chosen together with Rig product and alternative choices
-- with Rig sum.
chooseRig :: (Rig m) => Int -> [m] -> m
chooseRig 0 _ = one
chooseRig _ [] = zero
chooseRig k (opt:opts) = take .+. leave where
    take = opt .*. chooseRig (k-1) opts
    leave = chooseRig k opts

-- Set of values combined monoidally and deduplicated
newtype SetOf m = SetOf { asSet :: (S.Set m) }

instance (Eq m, Ord m, Monoid m) => Rig (SetOf m) where
    zero = SetOf S.empty
    one = SetOf $ S.singleton mempty
    (SetOf s1) .+. (SetOf s2) = SetOf $ s1 `S.union` s2
    (SetOf s1) .*. (SetOf s2) = foldl' (.+.) zero options where
        options = map mk_opt $ S.toList s1
        mk_opt m = SetOf $ S.map (mappend m) s2

-- Set of values m annotated with annotations ann.  Combination is
-- monoidal with deduplication on m, with by Rig operations on ann.
newtype Annotated m ann = Annotated (M.Map m ann)

instance (Eq m, Ord m, Monoid m, Rig ann) => Rig (Annotated m ann) where
    zero = Annotated $ M.empty
    one = Annotated $ M.singleton mempty one
    (Annotated m1) .+. (Annotated m2) = Annotated $ M.unionWith (.+.) m1 m2
    (Annotated m1) .*. (Annotated m2) = foldl' (.+.) zero options where
        options = map mk_opt $ M.toList m1
        mk_opt (m, ann) = Annotated $ M.mapKeysWith (.+.) (mappend m) $ M.map (ann .*.) m2
