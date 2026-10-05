module Ashfall.Robot where

data RobotStatus
  = Idle
  | Mining
  | Exploring
  | Charging
  | Repairing
  deriving (Show, Eq)

data Robot = Robot
  { robotId       :: Int
  , robotName     :: String
  , robotEnergy   :: Int
  , robotStatus   :: RobotStatus
  }

instance Show Robot where
  show (Robot rid name energy status) =
    "-- " ++ show name ++ " [" ++ show rid ++ "] --\n" ++
    "Energy: " ++ show energy ++
    "\nStatus: " ++ show status

instance Eq Robot where
  (Robot id1 _ _ _) == (Robot id2 _ _ _) =
    id1 == id2

isWorking :: RobotStatus -> Bool
isWorking Idle = False
isWorking Mining = True
isWorking Exploring = True
isWorking Charging = False
isWorking Repairing = False

consumeEnergy :: Int -> Robot -> Either String Robot
consumeEnergy x robot
  | x < 0 = Left "> Invalid energy amount!"
  | x > robotEnergy robot = Left "> Robot is out of energy!"
  | otherwise = Right (robot { robotEnergy = robotEnergy robot - x })