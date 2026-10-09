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