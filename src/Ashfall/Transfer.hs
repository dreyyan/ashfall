module Ashfall.Transfer
  ( depositResources
  ) where

import Ashfall.Robot
import Ashfall.Base
import Ashfall.Resource
import Ashfall.Error

-- | Move resources from robot to base.
depositResources :: Robot -> Resource -> Int -> Base -> Either GameError (Robot, Base)
depositResources robot resource n base = do
  updatedRobot <- removeFromInventory robot resource n
  updatedBase <- addResources base resource n

  Right (updatedRobot, updatedBase)