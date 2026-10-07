module Main (main) where

import Test.Hspec
import Ashfall.Robot
import Ashfall.Error

-- Test fixtures
readyRobot :: Robot
readyRobot = Robot 1 "Adam" 100 Idle (Just Explore)

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


main :: IO()
main = hspec $ do
  describe "consumeEnergy" $ do
    it "accepts valid energy consumption" $ do
      consumeEnergy 10 readyRobot
        `shouldBe` Right (readyRobot { robotEnergy = 90 })
    it "accepts full energy consumption" $ do
      consumeEnergy 100 readyRobot
        `shouldBe` Right noEnergyRobot
    it "accepts zero energy consumption" $ do
      consumeEnergy 0 readyRobot
        `shouldBe` Right readyRobot
    it "rejects negative amounts" $ do
      consumeEnergy (-10) readyRobot
        `shouldBe` Left InvalidEnergyAmount
    it "rejects if robot has insufficient energy" $ do
      consumeEnergy 101 readyRobot
        `shouldBe` Left InsufficientEnergy

  describe "rechargeEnergy" $ do
    it "accepts valid energy recharge" $ do
      rechargeEnergy (10) lowEnergyRobot
        `shouldBe` Right readyRobot
    it "recharging above maximum caps at 100" $ do
      rechargeEnergy 100 lowEnergyRobot
        `shouldBe` Right readyRobot
    it "rejects non-positive amounts" $ do
      rechargeEnergy (-1) lowEnergyRobot
        `shouldBe` Left InvalidEnergyAmount
    it "rejects zero recharge amount" $ do
      rechargeEnergy (0) lowEnergyRobot
        `shouldBe` Left InvalidEnergyAmount

  describe "taskAssignment" $ do
    describe "assignTask" $ do
      it "should assign a task successfully" $ do
        assignTask Explore untaskedRobot
          `shouldBe` readyRobot
      it "should assign a task to disabled robot" $ do
        assignTask Mine disabledRobot
          `shouldBe` disabledRobot { robotTask = Just Mine }
      it "should replace existing task" $ do
        assignTask Mine readyRobot
          `shouldBe` readyRobot { robotTask = Just Mine }

    describe "unassignTask" $ do
      it "should unassign a task successfully" $ do
        unassignTask readyRobot
          `shouldBe` Right untaskedRobot
      it "should reject if no assigned task" $ do
        unassignTask untaskedRobot
          `shouldBe` Left NoTaskAssigned

  describe "canExecuteTask" $ do
    it "allows a ready robot to execute a task" $ do
      canExecuteTask readyRobot
        `shouldBe` Right ()
    it "should reject an already working robot" $ do
      canExecuteTask workingRobot
        `shouldBe` Left AlreadyWorking
    it "should reject a disabled robot" $ do
      canExecuteTask disabledRobot
        `shouldBe` Left DisabledRobot
    it "should reject a robot with no energy" $ do
      canExecuteTask noEnergyRobot
        `shouldBe` Left InsufficientEnergy
    it "should reject an untasked robot" $ do
      canExecuteTask untaskedRobot
        `shouldBe` Left NoTaskAssigned

  describe "executeTask" $ do
    it "should execute task successfully" $ do
      executeTask readyRobot
        `shouldBe` Right workingRobot
    it "should reject an already working robot" $ do
      executeTask workingRobot
        `shouldBe` Left AlreadyWorking
    it "should reject if robot is disabled" $ do
      executeTask disabledRobot
        `shouldBe` Left DisabledRobot
    it "should reject if robot has no energy" $ do
      executeTask noEnergyRobot
        `shouldBe` Left InsufficientEnergy
    it "should reject if robot has no task assigned" $ do
      executeTask untaskedRobot
        `shouldBe` Left NoTaskAssigned

  describe "errorMessage" $ do
    it "formats invalid energy errors" $ do
      errorMessage InvalidEnergyAmount
        `shouldBe` "> Invalid energy amount!"

    it "formats insufficient energy errors" $ do
      errorMessage InsufficientEnergy
        `shouldBe` "> Insufficient energy!"

    it "formats no task assigned errors" $ do
      errorMessage NoTaskAssigned
        `shouldBe` "> No task assigned!"

    it "formats no robot disabled errors" $ do
      errorMessage DisabledRobot
        `shouldBe` "> Robot is disabled!"

    it "formats already working errors" $ do
      errorMessage AlreadyWorking
        `shouldBe` "> Robot is already working!"