module Ashfall.Base where

import Ashfall.Error
import Ashfall.Resource

-- | Represents the player's base.
data Base = Base
  { baseName      :: String
  , baseStorage   :: Storage
  , baseCapacity  :: Int
  } deriving (Show, Eq)

-- | Represents the player's current storage.
data Storage = Storage
  { scrap     :: Int
  , iron      :: Int
  , crystal   :: Int
  , fuel      :: Int
  } deriving (Show, Eq)

-- | Add resources to the player's base.
addResources :: Resource -> Int -> Base -> Either GameError Base
addResources resource amount base = do
  let isValidAmount = amount > 0

  if not isValidAmount
    then Left InvalidResourceAmount
    else do
      let oldStorage = baseStorage base
          newStorage =
            case resource of
              Scrap   -> oldStorage { scrap = scrap oldStorage + amount }
              Iron    -> oldStorage { iron = iron oldStorage + amount }
              Crystal -> oldStorage { crystal = crystal oldStorage + amount }
              Fuel    -> oldStorage { fuel = fuel oldStorage + amount }

      Right base { baseStorage = newStorage }