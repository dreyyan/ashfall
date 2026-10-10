module Ashfall.Robot where

import Ashfall.Constants (maxRobotEnergy)
import Ashfall.Error
import Ashfall.Resource
import Ashfall.Storage

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
  , robotCapacity :: Int
  , robotStorage  :: Storage
  }

instance Show Robot where
  show (Robot rid name energy status task capacity storage) =
    "-- " ++ name ++ " [" ++ show rid ++ "] --" ++
    "\nEnergy: " ++ show energy ++
    "\nStatus: " ++ show status ++
    "\nTask: " ++ show task ++
    "\nCapacity: " ++ show capacity ++
    "\nStorage: " ++ show storage

instance Eq Robot where
  (Robot id1 _ _ _ _ _ _) == (Robot id2 _ _ _ _ _ _) =
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

-- | Checks whether a robot is capable of starting its assigned task.
canStartTask :: Robot -> Either GameError ()
canStartTask robot
  | robotStatus robot == Disabled = Left DisabledRobot
  | robotStatus robot == Working = Left AlreadyWorking
  | robotEnergy robot <= 0 = Left InsufficientEnergy
  | robotTask robot == Nothing = Left NoTaskAssigned
  | otherwise = Right ()

-- | Start a robot's assigned task.
startTask :: Robot -> Either GameError Robot
startTask robot = do
  _ <- canStartTask robot -- if robot cannot execute task, `Left` error propagates.
  Right robot { robotStatus = Working }

-- | Checks whether a robot is currently working.
isBusy :: Robot -> Bool
isBusy robot = if robotStatus robot == Working then True else False

-- | Stop a robot's assigned task.
stopTask :: Robot -> Either GameError Robot
stopTask robot =
  -- Check if robot is currently working on a task.
  if isBusy robot == False
    then Left NotWorking
    else Right robot { robotStatus = Idle }

-- | Calculate robot's remaining capacity.
robotRemainingCapacity :: Robot -> Int
robotRemainingCapacity robot =
  robotCapacity robot - storageUsed (robotStorage robot)

-- | Add resources to the robot's inventory.
addToInventory :: Resource -> Int -> Robot -> Either GameError Robot
addToInventory resource n robot = do
  newStorage <- addToStorage resource n (robotStorage robot)

  if not (canStore resource n (robotCapacity robot) (robotStorage robot))
    then Left InsufficientCapacity
    else Right robot { robotStorage = newStorage }

-- | Remove resouces from the robot's inventory.
removeFromInventory :: Resource -> Int -> Robot -> Either GameError Robot
removeFromInventory resource n robot = do
  newStorage <- removeFromStorage resource n (robotStorage robot)
  Right robot { robotStorage = newStorage }

-- | Performs the robot's assigned task.
-- performTask :: Robot -> _
-- performTask robot =
--   case robotTask robot of
--     Mine -> 
--     Explore -> 
--     Recharge -> 
--     Repair -> 