module Ashfall.Robot where
import Ashfall.Constants (maxRobotEnergy)
import Ashfall.Error (GameError (InvalidEnergyAmount, InsufficientEnergy))

-- | Represents a Robot's status.
data RobotStatus
  = Idle
  | Mining
  | Exploring
  | Charging
  | Repairing
  deriving (Show, Eq)

-- | Represents a task the Robot performs.
data RobotTask
  = Mine
  | Explore
  | Recharge
  | Repair
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
consumeEnergy :: Int -> Robot -> Either GameError Robot
consumeEnergy x robot
  | x < 0 = Left (InvalidEnergyAmount)
  | x > robotEnergy robot = Left (InsufficientEnergy)
  | otherwise = Right (robot { robotEnergy = robotEnergy robot - x })

-- | Recharges a robot's energy.
rechargeEnergy :: Int -> Robot -> Either GameError Robot
rechargeEnergy x robot
  | x < 1 = Left (InvalidEnergyAmount)
  | otherwise = Right (robot { robotEnergy = min maxRobotEnergy (robotEnergy robot + x) })