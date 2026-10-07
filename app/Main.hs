module Main (main) where
import Ashfall.Robot

main :: IO ()
main = do
  let newRobot = Robot 1 "Adam" 100 Idle (Just Explore)
  print (newRobot)