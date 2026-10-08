module Ashfall.Resource where

-- | Represents a resource in Ashfall.
data Resource
  = Scrap
  | Iron
  | Crystal
  | Fuel
  deriving (Show, Eq, Ord)

-- | Retrieve a resource's description.
resourceDescription :: Resource -> String
resourceDescription Scrap     = "Salvaged metal and machinery parts."
resourceDescription Iron      = "A durable metal used for construction."
resourceDescription Crystal   = "A rare crystalline material."
resourceDescription Fuel      = "A combustible resource used to power machinery."

-- | Retrieve how much capacity a resource costs.
resourceCapacityCost :: Resource -> Int
resourceCapacityCost Scrap    = 1
resourceCapacityCost Iron     = 2
resourceCapacityCost Crystal  = 3
resourceCapacityCost Fuel     = 2