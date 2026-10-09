module Fixtures where

import Ashfall.Base
import Ashfall.Resource
import Ashfall.Robot

import qualified Data.Map.Strict as Map
import Ashfall.Constants (robotDefaultCapacity)

-- Test fixtures: Robot
readyRobot :: Robot
readyRobot = Robot 1 "Adam" 100 Idle (Just Explore) robotDefaultCapacity (Storage Map.empty)

lowEnergyRobot :: Robot
lowEnergyRobot = readyRobot { robotEnergy = 90 }

noEnergyRobot :: Robot
noEnergyRobot = readyRobot { robotEnergy = 0 }

untaskedRobot :: Robot
untaskedRobot = readyRobot { robotTask = Nothing }

disabledRobot :: Robot
disabledRobot = readyRobot { robotStatus = Disabled }

workingRobot :: Robot
workingRobot = readyRobot { robotStatus = Working }

-- Test fixtures: Base
defaultBase :: Base
defaultBase =
  Base
    { baseName = "Default"
    , baseStorage = Storage Map.empty
    , baseCapacity = 100
    }

richBase :: Base
richBase =
  Base
    { baseName = "Default"
    , baseStorage =
        Storage
          (Map.fromList
            [ (Scrap, 20)
            , (Iron, 15)
            , (Crystal, 10)
          , (Fuel, 10)
            ])
    , baseCapacity = 100
    }