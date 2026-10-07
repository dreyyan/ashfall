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
      it "assigns a task successfully" $ do
        assignTask Explore untaskedRobot
          `shouldBe` readyRobot
      it "assigns a task to disabled robot" $ do
        assignTask Mine disabledRobot
          `shouldBe` disabledRobot { robotTask = Just Mine }
      it "replaces existing task" $ do
        assignTask Mine readyRobot
          `shouldBe` readyRobot { robotTask = Just Mine }

    describe "unassignTask" $ do
      it "unassigns a task successfully" $ do
        unassignTask readyRobot
          `shouldBe` Right untaskedRobot
      it "rejects if no assigned task" $ do
        unassignTask untaskedRobot
          `shouldBe` Left NoTaskAssigned

  describe "canStartTask" $ do
    it "allows a ready robot to start a task" $ do
      canStartTask readyRobot
        `shouldBe` Right ()
    it "rejects an already working robot" $ do
      canStartTask workingRobot
        `shouldBe` Left AlreadyWorking
    it "rejects a disabled robot" $ do
      canStartTask disabledRobot
        `shouldBe` Left DisabledRobot
    it "rejects a robot with no energy" $ do
      canStartTask noEnergyRobot
        `shouldBe` Left InsufficientEnergy
    it "rejects an untasked robot" $ do
      canStartTask untaskedRobot
        `shouldBe` Left NoTaskAssigned

  describe "startTask" $ do
    it "starts a working robot" $ do
      startTask readyRobot
        `shouldBe` Right workingRobot
    it "rejects an already working robot" $ do
      startTask workingRobot
        `shouldBe` Left AlreadyWorking
    it "rejects if robot is disabled" $ do
      startTask disabledRobot
        `shouldBe` Left DisabledRobot
    it "rejects if robot has no energy" $ do
      startTask noEnergyRobot
        `shouldBe` Left InsufficientEnergy
    it "rejects if robot has no task assigned" $ do
      startTask untaskedRobot
        `shouldBe` Left NoTaskAssigned

  describe "isBusy" $ do
    it "returns true if robot is working" $ do
      isBusy workingRobot
        `shouldBe` True
    it "returns false if robot is not working" $ do
      isBusy readyRobot
        `shouldBe` False

  describe "stopTask" $ do
    it "stops a working robot" $ do
      stopTask workingRobot
        `shouldBe` Right readyRobot    
    it "rejects if robot is not working" $ do
      stopTask readyRobot
        `shouldBe` Left NotWorking
    it "keeps the assigned task after stopping" $ do
      stopTask workingRobot
        `shouldBe` Right workingRobot { robotTask = Just Explore }

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