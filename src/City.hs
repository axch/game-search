{-# LANGUAGE TemplateHaskell #-}

module City where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M

data Unit = Settler | Militia

data Resource = Food | Shield | Trade | Gold | Bulb

newtype Stack a = Stack (M.Map a Int)
type Resources = Stack Resource

data Tile = Tile { production :: Resources }

data City = City { _center :: Tile
                 , _available :: [Tile]
                 , _pop :: Int
                 , _stored :: Resources
                 }

makeLenses ''City

-- Mechanics of growth and production
-- Ignore output of gold or research for now
grow :: Resources -> Unit -> City -> (Maybe Unit, City)
grow increment order city = runState act city where
    act :: State City (Maybe Unit)
    act = do
      stored += increment
      return Nothing
