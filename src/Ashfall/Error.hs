module Ashfall.Error where

data GameError
  = InvalidEnergyAmount
  | InsufficientEnergy
  | NoTaskAssigned
  deriving (Eq, Show)

errorMessage :: GameError -> String
errorMessage InvalidEnergyAmount = "> " ++ "Invalid energy amount!"
errorMessage InsufficientEnergy = "> " ++ "Insufficient energy!"
errorMessage NoTaskAssigned = "> " ++ "No task assigned!"