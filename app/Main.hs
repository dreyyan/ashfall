module Main (main) where

import Ashfall.Robot
import Ashfall.Constants (robotDefaultCapacity)
import Ashfall.Storage

import qualified Data.Map.Strict as Map
import Ashfall.Equipment (MiningEquipment(BasicDrill))

main :: IO ()
main = do
  let newRobot = Robot 1 "Adam" 100 Idle (Just Explore) robotDefaultCapacity (Storage Map.empty) BasicDrill
  print (newRobot)