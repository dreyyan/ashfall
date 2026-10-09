module Ashfall.Base where

import Ashfall.Error
import Ashfall.Resource
import Ashfall.Storage

import qualified Data.Map.Strict as Map

-- | Represents the player's base.
data Base = Base
  { baseName      :: String
  , baseStorage   :: Storage
  , baseCapacity  :: Int
  } deriving (Show, Eq)

-- | Add resources to the player's base.
addResources :: Resource -> Int -> Base -> Either GameError Base
addResources resource amount base = do
  let isValidAmount = amount > 0

  if not isValidAmount
    then Left InvalidResourceAmount
    else do
      let oldStorage = baseStorage base
          oldResources = resources oldStorage
          currentAmount = Map.findWithDefault 0 resource oldResources
          newAmount = currentAmount + amount
          newResources = Map.insert resource newAmount oldResources
          newStorage = Storage newResources;

      Right base { baseStorage = newStorage }

-- | Consumes resouces from the player's base.
consumeResources :: Resource -> Int -> Base -> Either GameError Base
consumeResources resource amount base = do
  let isValidAmount = amount > 0

  if not isValidAmount
    then Left InvalidResourceAmount
    else do
      let oldStorage = baseStorage base
          oldResources = resources oldStorage
          currentAmount = Map.findWithDefault 0 resource oldResources
          newAmount = currentAmount - amount
          newResources = Map.insert resource newAmount oldResources
          newStorage = Storage newResources;

      Right base { baseStorage = newStorage }

-- | Calculate the capcaity cost of a resource amount.
calculateCost :: (Resource, Int) -> Int
calculateCost (resource, amount) =
  amount * resourceCapacityCost resource

-- | Calculate how much storage is used.
storageUsed :: Storage -> Int
storageUsed storage =
  let resourceList = Map.toList (resources storage)
  in sum (map calculateCost resourceList)

-- | Calculate remaining capcity.
remainingCapacity :: Base -> Int
remainingCapacity base = baseCapacity base - storageUsed (baseStorage base)

-- | Checks whether base can store `n` `resource`.
canStore :: Resource -> Int -> Base -> Bool
canStore resource n base =
  n > 0 && calculateCost (resource, n) <= remainingCapacity base