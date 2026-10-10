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