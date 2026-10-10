module Ashfall.Location where

import Ashfall.Resource

import Data.Map.Strict (Map)

-- | Represents a location in Ashfall.
data Location = Location
  { locationId            :: String
  , locationName          :: String
  , locationDescription   :: String
  , locationType          :: LocationType
  , locationResources     :: Map Resource Int
  } deriving (Show, Eq)

-- | Represents a location's type.
data LocationType
  = Scrapyard       -- 75% Scrap / 20% Iron / 5% Fuel
  | IronQuarry      -- 20% Scrap / 75% Iron / 5% Crystal
  | CrystalCavern   -- 10% Scrap / 30% Iron / 60% Crystal
  | GasStation      -- 20% Scrap / 10% Iron / 70% Fuel
  deriving (Eq, Ord, Show)

scrapyard :: Location
scrapyard = undefined

ironQuarry :: Location
ironQuarry = undefined

crystalCavern :: Location
crystalCavern = undefined

gasStation :: Location
gasStation = undefined

-- | Returns the probability (0-1100%) of finding a resource at a given location.
getResourceChance :: Resource -> Location -> Int
getResourceChance resource location = undefined

-- | Returns all available resources that can be found at a given location.
availableResources :: Location -> [Resource]
availableResources location = undefined

-- | Determines whether a resource can be found at a given location.
canFindResource :: Resource -> Location -> Bool
canFindResource resource location = undefined

-- | Sums up all resource probabilities in a location.
totalResourceChance :: Location -> Int
totalResourceChance location = undefined

-- | Checks whether existing location is valid.
-- Every probability must be between 0 and 100.
-- All probabilities must sum to exactly 100.
-- A location must contain at least one resource with a positive probability.
validateLocation :: Location -> Bool
validateLocation location = undefined