module Ashfall.Storage where

import Ashfall.Resource
import Ashfall.Error

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Represents the player's current storage.
newtype Storage = Storage
  { resources :: Map Resource Int }
  deriving (Show, Eq)

-- | Add resources to storage.
addToStorage :: Resource -> Int -> Storage -> Either GameError Storage
addToStorage resource amount storage = do
  let isValidAmount = amount > 0

  if not isValidAmount
    then Left InvalidResourceAmount
    else do
      let oldResources = resources storage
          currentAmount = Map.findWithDefault 0 resource oldResources
          newAmount = currentAmount + amount
          newResources = Map.insert resource newAmount oldResources

      Right (Storage newResources)

-- | Remove resources from storage.
removeFromStorage :: Resource -> Int -> Storage -> Either GameError Storage
removeFromStorage resource amount storage = do
  let isValidAmount = amount > 0

  if not isValidAmount
    then Left InvalidResourceAmount
    else do
      let oldResources = resources storage
          currentAmount = Map.findWithDefault 0 resource oldResources
          hasEnoughResources = currentAmount >= amount

      if not hasEnoughResources
        then Left InsufficientResources
        else do
          let newAmount = currentAmount - amount
              newResources = Map.insert resource newAmount oldResources

          Right (Storage newResources)

-- | Calculate the capcaity cost of a resource amount.
calculateCost :: (Resource, Int) -> Int
calculateCost (resource, amount) =
  amount * resourceCapacityCost resource

-- | Calculate how much storage is used.
storageUsed :: Storage -> Int
storageUsed storage =
  let resourceList = Map.toList (resources storage)
  in sum (map calculateCost resourceList)
  
-- | Checks whether storage has enough remaining capacity.
canStore :: Resource -> Int -> Int -> Storage -> Bool
canStore resource amount capacity storage =
  amount > 0 &&
  calculateCost (resource, amount) <= capacity - storageUsed storage