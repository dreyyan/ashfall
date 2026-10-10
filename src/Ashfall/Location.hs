module Ashfall.Location where

import Ashfall.Resource

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

-- | Retrieves the resource drop weights for a given location type.
resourceWeights :: LocationType -> [(Resource, Int)]
resourceWeights Scrapyard =
  [(Scrap, 75), (Iron, 20), (Fuel, 5)]

resourceWeights IronQuarry =
  [(Scrap, 20), (Iron, 75), (Crystal, 5)]

resourceWeights CrystalCavern =
  [(Scrap, 10), (Iron, 30), (Crystal, 60)]

resourceWeights GasStation =
  [(Scrap, 20), (Iron, 10), (Fuel, 70)]

scrapyard :: Location
scrapyard = Location
  { locationId = "LOC-001"
  , locationName = "Scrapyard"
  , locationDescription = "A vast field of rusted machinery and abandoned robots, rich in salvageable scrap."
  , locationType = Scrapyard
  , locationResources = Map.fromList
      [ (Scrap, 75)
      , (Iron, 20)
      , (Fuel, 5)
      ]
  }

ironQuarry :: Location
ironQuarry = Location
  { locationId = "LOC-002"
  , locationName = "Iron Quarry"
  , locationDescription = "An abandoned mining site with exposed iron deposits buried beneath fractured rock."
  , locationType = IronQuarry
  , locationResources = Map.fromList
      [ (Scrap, 20)
      , (Iron, 75)
      , (Crystal, 5)
      ]
  }

crystalCavern :: Location
crystalCavern = Location
  { locationId = "LOC-003"
  , locationName = "Crystal Cavern"
  , locationDescription = "A subterranean cave illuminated by rare crystalline formations."
  , locationType = CrystalCavern
  , locationResources = Map.fromList
      [ (Scrap, 10)
      , (Iron, 30)
      , (Crystal, 60)
      ]
  }

gasStation :: Location
gasStation = Location
  { locationId = "LOC-004"
  , locationName = "Gas Station"
  , locationDescription = "A derelict refueling station with partially intact underground fuel reserves."
  , locationType = GasStation
  , locationResources = Map.fromList
      [ (Scrap, 20)
      , (Iron, 10)
      , (Fuel, 70)
      ]
  }

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