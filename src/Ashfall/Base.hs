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
addResources :: Base -> Resource -> Int -> Either GameError Base
addResources base resource n = do
  newStorage <- addToStorage (baseStorage base) resource n

  if not (canStore resource n (baseCapacity base) (baseStorage base))
    then Left InsufficientCapacity
    else Right base { baseStorage = newStorage }

-- | Consumes resouces from the player's base.
consumeResources :: Base -> Resource -> Int -> Either GameError Base
consumeResources base resource n = do
  newStorage <- removeFromStorage (baseStorage base) resource n
  Right base { baseStorage = newStorage }

-- | Calculate remaining capcity.
remainingCapacity :: Base -> Int
remainingCapacity base = baseCapacity base - storageUsed (baseStorage base)