module Ashfall.Robot where
import Ashfall.Constants (maxRobotEnergy)
import Ashfall.Error

-- | Represents a Robot's status.
data RobotStatus
  = Working
  | Idle
  | Disabled
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
  , robotTask     :: Maybe RobotTask
  }

instance Show Robot where
  show (Robot rid name energy status task) =
    "-- " ++ name ++ " [" ++ show rid ++ "] --" ++
    "\nEnergy: " ++ show energy ++
    "\nStatus: " ++ show status ++
    "\nTask: " ++ show task

instance Eq Robot where
  (Robot id1 _ _ _ _) == (Robot id2 _ _ _ _) =
    id1 == id2

-- | Consumes energy from a Robot.
consumeEnergy :: Int -> Robot -> Either GameError Robot
consumeEnergy x robot
  | x < 0 = Left InvalidEnergyAmount
  | x > robotEnergy robot = Left InsufficientEnergy
  | otherwise = Right (robot { robotEnergy = robotEnergy robot - x })

-- | Recharges a robot's energy.
rechargeEnergy :: Int -> Robot -> Either GameError Robot
rechargeEnergy x robot
  | x < 1 = Left InvalidEnergyAmount
  | otherwise = Right (robot { robotEnergy = min maxRobotEnergy (robotEnergy robot + x) })

-- | Assign a specific task to a robot.
assignTask :: RobotTask -> Robot -> Robot
assignTask task robot = robot { robotTask = Just task }

-- | Unassign task from a robot.
unassignTask :: Robot -> Either GameError Robot
unassignTask robot =
  case robotTask robot of
    Nothing -> Left NoTaskAssigned
    _ -> Right robot { robotTask = Nothing }

-- | Checks whether a robot is capable of performing its assigned task.
canExecuteTask :: Robot -> Either GameError ()
canExecuteTask robot
  | robotStatus robot == Disabled = Left DisabledRobot
  | robotStatus robot == Working = Left AlreadyWorking
  | robotEnergy robot <= 0 = Left InsufficientEnergy
  | robotTask robot == Nothing = Left NoTaskAssigned
  | otherwise = Right ()

-- | Execute a robot's assigned task.
executeTask :: Robot -> Either GameError Robot
executeTask robot = do
  _ <- canExecuteTask robot -- if robot cannot execute task, `Left` error propagates.
  Right robot { robotStatus = Working }