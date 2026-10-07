module Ashfall.Base where

import Ashfall.Error

-- | Represents the player's base.
data Base = Base
{ baseName    :: String
, baseStorage :: Storage
, baseCapcity :: Int
} deriving (Show, Eq)

-- | Represents the player's current storage.
data Storage = Storage
{ scrap     :: Int
, iron      :: Int
, crystal   :: Int
, fuel      :: Int
} deriving (Show, Eq)