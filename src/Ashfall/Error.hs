module Ashfall.Error where

data GameError
  = InvalidEnergyAmount
  | InsufficientEnergy
  | NoTaskAssigned
  | DisabledRobot
  | AlreadyWorking
  | NotWorking
  | InvalidResourceAmount
  | InsufficientResources
  | InsufficientCapacity
  | ResourceUnavailable
  | UnsupportedResource
  deriving (Eq, Show)

errorMessage :: GameError -> String
errorMessage InvalidEnergyAmount = "> " ++ "Invalid energy amount!"
errorMessage InsufficientEnergy = "> " ++ "Insufficient energy!"
errorMessage NoTaskAssigned = "> " ++ "No task assigned!"
errorMessage DisabledRobot = "> " ++ "Robot is disabled!"
errorMessage AlreadyWorking = "> " ++ "Robot is already working!"
errorMessage NotWorking = "> " ++ "Robot is not working!"
errorMessage InvalidResourceAmount = "> " ++ "Invalid resource amount!"
errorMessage InsufficientResources = "> " ++ "Insufficient resources!"
errorMessage InsufficientCapacity = "> " ++ "Insufficient capacity!"
errorMessage ResourceUnavailable = "> Resource not found at this location!"
errorMessage UnsupportedResource = "> Mining equipment cannot extract this resource!"