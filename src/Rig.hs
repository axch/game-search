{-# LANGUAGE FlexibleInstances #-}

-- Wanted to use Kmett's algebra package, but it wouldn't install

module Rig where

-- |
-- A Ring without (n)egation.
-- An additive commutative monoid with identity 'zero':
--
-- >         a .+. b  ==  b .+. a
-- >      zero .+. a  ==  a
-- > (a .+. b) .+. c  ==  a .+. (b .+. c)
--
-- A multiplicative monoid with identity 'one':
--
-- >        one .*. a  ==  a
-- >        a .*. one  ==  a
-- >  (a .*. b) .*. c  ==  a .*. (b .*. c)
--
-- Multiplication distributes over addition:
--
-- > a .*. (b .+. c)  ==  (a .*. b) .+. (a .*. c)
-- > (a .+. b) .*. c  ==  (a .*. c) .+. (b .*. c)
--
-- 'zero' annihilates a semiring with respect to multiplication:
--
-- > zero .*. a  ==  zero
-- > a .*. zero  ==  zero
class Rig s where
    zero, one    :: s
    (.+.), (.*.) :: s -> s -> s

newtype NumRig a = NumRig a

instance Num a => Rig (NumRig a) where
    zero = NumRig 0
    one = NumRig 1
    (NumRig x) .+. (NumRig y) = NumRig $ x + y
    (NumRig x) .*. (NumRig y) = NumRig $ x * y

instance Rig () where
    zero = ()
    one = ()
    _ .+. _ = ()
    _ .*. _ = ()

instance Rig Bool where
    zero = False
    one = True
    (.+.) = (||)
    (.*.) = (&&)

instance (Rig a, Rig b) => Rig (a, b) where
    zero = (zero, zero)
    one = (one, one)
    (a1, b1) .+. (a2, b2) = (a1 .+. a2, b1 .+. b2)
    (a1, b1) .*. (a2, b2) = (a1 .*. a2, b1 .*. b2)

newtype MaxPlus a = MaxPlus a
    deriving (Eq, Ord, Show)

-- This is not free: the monoid operation needs to distribute over the
-- order.
instance (Bounded a, Ord a, Monoid a) => Rig (MaxPlus a) where
    zero = MaxPlus minBound
    one = MaxPlus mempty
    (MaxPlus a1) .+. (MaxPlus a2) = MaxPlus $ a1 `max` a2
    (MaxPlus a1) .*. (MaxPlus a2) = MaxPlus $ a1 `mappend` a2

data MaxPlusA r ann = MaxPlusA r ann
    deriving (Eq, Ord, Show)

instance (Bounded r, Ord r, Monoid r, Rig ann) => Rig (MaxPlusA r ann) where
    zero = MaxPlusA minBound zero
    one = MaxPlusA mempty one
    (MaxPlusA r1 ann1) .+. (MaxPlusA r2 ann2) =
        MaxPlusA (r1 `max` r2) $ ann1 .+. ann2
    (MaxPlusA r1 ann1) .*. (MaxPlusA r2 ann2) =
        MaxPlusA (r1 `mappend` r2) $ ann1 .*. ann2
