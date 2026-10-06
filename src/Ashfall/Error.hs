module Ashfall.Error where

data GameError
  = InvalidEnergyAmount
  | InsufficientEnergy
  deriving (Eq, Show)

errorMessage :: GameError -> String
errorMessage InvalidEnergyAmount = "> " ++ "Invalid energy amount!"
errorMessage InsufficientEnergy = "> " ++ "Insufficient energy!"