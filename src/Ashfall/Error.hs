module Ashfall.Error where

data GameError
  = InvalidEnergyAmount
  | InsufficientEnergy
  | NoTaskAssigned
  | DisabledRobot
  | AlreadyWorking
  | NotWorking
  deriving (Eq, Show)

errorMessage :: GameError -> String
errorMessage InvalidEnergyAmount = "> " ++ "Invalid energy amount!"
errorMessage InsufficientEnergy = "> " ++ "Insufficient energy!"
errorMessage NoTaskAssigned = "> " ++ "No task assigned!"
errorMessage DisabledRobot = "> " ++ "Robot is disabled!"
errorMessage AlreadyWorking = "> " ++ "Robot is already working!"
errorMessage NotWorking = "> " ++ "Robot is not working!"