module Ashfall.Base where

import Ashfall.Error
import Ashfall.Resource
import Ashfall.Storage

-- | Represents the player's base.
data Base = Base
  { baseName      :: String
  , baseStorage   :: Storage
  , baseCapacity  :: Int
  } deriving (Show, Eq)

-- | Add resources to the player's base.
addResources :: Resource -> Int -> Base -> Either GameError Base
addResources resource n base = do
  newStorage <- addToStorage resource n (baseStorage base)

  if not (canStore resource n (baseCapacity base) (baseStorage base))
    then Left InsufficientCapacity
    else Right base { baseStorage = newStorage }

-- | Consumes resouces from the player's base.
consumeResources :: Resource -> Int -> Base -> Either GameError Base
consumeResources resource n base = do
  newStorage <- removeFromStorage resource n (baseStorage base)
  Right base { baseStorage = newStorage }

-- | Calculate remaining capcity.
remainingCapacity :: Base -> Int
remainingCapacity base = baseCapacity base - storageUsed (baseStorage base)