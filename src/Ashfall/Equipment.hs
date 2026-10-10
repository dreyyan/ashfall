module Ashfall.Equipment where
import Ashfall.Resource

-- | Represents a mining equipment a robot can use.
data MiningEquipment
  = BasicDrill
  | IndustrialDrill
  | AdvancedExtractor
  deriving (Eq, Ord, Show)

-- | Returns the energy required to operate mining equipment.
equipmentEnergyCost :: MiningEquipment -> Int
equipmentEnergyCost BasicDrill = 1
equipmentEnergyCost IndustrialDrill = 2
equipmentEnergyCost AdvancedExtractor = 3

-- | Returns the minimum and maximum mining yield.
equipmentYield :: MiningEquipment -> (Int, Int)
equipmentYield BasicDrill = (1, 1)
equipmentYield IndustrialDrill = (2, 4)
equipmentYield AdvancedExtractor = (4, 7)

-- | Returns the resources supported by mining equipment.
equipmentMineableResources :: MiningEquipment -> [Resource]
equipmentMineableResources BasicDrill = [Scrap]
equipmentMineableResources IndustrialDrill = [Scrap, Iron, Fuel]
equipmentMineableResources AdvancedExtractor = [Scrap, Iron, Crystal, Fuel]

-- | Determines whether equipment can mine a resource.
canMineResource :: Resource -> MiningEquipment -> Bool
canMineResource resource equipment =
  -- Checks whether the resource exists in the list of mineable resources.
  resource `elem` equipmentMineableResources equipment