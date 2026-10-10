module Ashfall.Mining where

import Ashfall.Resource
import Ashfall.Location
import Ashfall.Equipment
import Ashfall.Robot
import Ashfall.Error

import qualified Data.Map.Strict as Map
import Data.List (find)

-- | Determine which resource is mined based on set probabilities.
selectResource :: Location -> Int -> Maybe Resource
selectResource location roll
  | roll < 1 || roll > 100 = Nothing
  | not (validateLocation location) = Nothing
  | otherwise =
      let resources = Map.toList (locationResources location)
          probabilities = map snd resources
          cumulative = drop 1 (scanl (+) 0 probabilities)
          cumulativeResources = zip (map fst resources) cumulative
      in fmap fst (find (\(_, probability) -> probability >= roll) cumulativeResources)

-- | Calculates the mining yield based on equipment and a yield roll.
calculateMiningYield :: MiningEquipment -> Int -> Int
calculateMiningYield equipment roll =
  let (minYield, maxYield) = equipmentYield equipment
  in minYield + ((roll - 1) `mod` (maxYield - minYield + 1))

-- | Validates whether a robot can mine a resource at a location.
validateMining :: Robot -> Location -> Resource -> Either GameError ()
validateMining robot location resource
  | robotStatus robot == Disabled = Left DisabledRobot
  | robotTask robot /= Just Mine = Left NoTaskAssigned
  | not (canFindResource resource location) = Left ResourceUnavailable
  | not (canMineResource resource (robotEquipment robot)) = Left UnsupportedResource
  | robotEnergy robot < equipmentEnergyCost (robotEquipment robot) = Left InsufficientEnergy
  | otherwise = Right ()