module Ashfall.Resource where

import Ashfall.Error

-- | Represents a resource in Ashfall.
data Resource
  = Scrap
  | Iron
  | Crystal
  | Fuel
  deriving (Show, Eq)

-- | Retrieve a resource's description.
resourceDescription :: Resource -> String
resourceDescription Scrap     = "Salvaged metal and machinery parts."
resourceDescription Iron      = "A durable metal used for construction."
resourceDescription Crystal   = "A rare crystalline material."
resourceDescription Fuel      = "A combustible resource used to power machinery."