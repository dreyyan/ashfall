module Ashfall.Robot where
import Ashfall.Constants (maxRobotEnergy)

-- | Represents a Robot's status.
data RobotStatus
  = Idle
  | Mining
  | Exploring
  | Charging
  | Repairing
  deriving (Show, Eq)

-- | Represents a Robot in Ashfall.
data Robot = Robot
  { robotId       :: Int
  , robotName     :: String
  , robotEnergy   :: Int
  , robotStatus   :: RobotStatus
  }

instance Show Robot where
  show (Robot rid name energy status) =
    "-- " ++ name ++ " [" ++ show rid ++ "] --\n" ++
    "Energy: " ++ show energy ++
    "\nStatus: " ++ show status

instance Eq Robot where
  (Robot id1 _ _ _) == (Robot id2 _ _ _) =
    id1 == id2

-- | Check if Robot is working.
isWorking :: RobotStatus -> Bool
isWorking Idle = False
isWorking Mining = True
isWorking Exploring = True
isWorking Charging = False
isWorking Repairing = False

-- | Consumes energy from a Robot.
consumeEnergy :: Int -> Robot -> Either String Robot
consumeEnergy x robot
  | x < 0 = Left "> Invalid energy amount!"
  | x > robotEnergy robot = Left "> Robot is out of energy!"
  | otherwise = Right (robot { robotEnergy = robotEnergy robot - x })

-- | Recharges a robot's energy.
rechargeEnergy :: Int -> Robot -> Either String Robot
rechargeEnergy x robot
  | x < 1 = Left "> Invalid recharge amount!"
  | otherwise = Right (robot { robotEnergy = min maxRobotEnergy (robotEnergy robot + x) })