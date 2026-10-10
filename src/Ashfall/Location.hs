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

-- | Returns the probability (0-100%) of finding a resource at a given location.
getResourceChance :: Resource -> Location -> Int
getResourceChance resource location =
 Map.findWithDefault 0 resource (locationResources location)

-- | Returns all available resources that can be found at a given location.
availableResources :: Location -> [Resource]
availableResources location =
  -- Only include resources with a positive probability.
  Map.keys (Map.filter (> 0) (locationResources location))

-- | Determines whether a resource can be found at a given location.
canFindResource :: Resource -> Location -> Bool
canFindResource resource location =
  Map.findWithDefault 0 resource (locationResources location) > 0

-- | Sums up all resource probabilities in a location.
totalResourceChance :: Location -> Int
totalResourceChance location =
  sum (Map.elems (locationResources location))

-- | Determines whether a location has a valid resource probability distribution.
validateLocation :: Location -> Bool
validateLocation location =
  let probabilities = Map.elems (locationResources location)
  in all (\chance -> chance >= 0 && chance <= 100) probabilities
     && totalResourceChance location == 100
     && any (> 0) probabilities