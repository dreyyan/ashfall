module Fixtures where

import Ashfall.Base
import Ashfall.Constants (robotDefaultCapacity)
import Ashfall.Resource
import Ashfall.Robot
import Ashfall.Storage

import qualified Data.Map.Strict as Map

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

loadedRobot :: Robot
loadedRobot =
  readyRobot
    { robotStorage =
        Storage
          (Map.fromList
            [ (Scrap, 5)
            , (Iron, 2)
            , (Crystal, 1)
            ])
    }

fullRobot :: Robot
fullRobot =
  readyRobot
    { robotStorage =
        Storage (Map.singleton Scrap robotDefaultCapacity)
    }

exactLimitRobot :: Robot
exactLimitRobot =
  readyRobot
    { robotEnergy = 10
    , robotStorage =
        Storage (Map.singleton Scrap (robotDefaultCapacity - 1))
    }

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

-- Test fixtures: Storage
emptyStorage :: Storage
emptyStorage = Storage Map.empty

smallStorage :: Storage
smallStorage =
  Storage
    (Map.fromList
      [ (Scrap, 5)
      , (Iron, 3)
      , (Crystal, 2)
      , (Fuel, 1)
      ])

richStorage :: Storage
richStorage =
  Storage
    (Map.fromList
      [ (Scrap, 20)
      , (Iron, 15)
      , (Crystal, 10)
      , (Fuel, 10)
      ])

fullStorage :: Storage
fullStorage =
  Storage
    (Map.fromList
      [ (Scrap, 100)
      ])