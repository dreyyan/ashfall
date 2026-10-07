module Main (main) where

import Test.Hspec
import Ashfall.Robot
import Ashfall.Error

-- Test fixtures
fullEnergyRobot :: Robot
fullEnergyRobot = Robot 1 "Adam" 100 Idle (Just Explore)

lowEnergyRobot :: Robot
lowEnergyRobot = fullEnergyRobot { robotEnergy = 90 }

noEnergyRobot :: Robot
noEnergyRobot = fullEnergyRobot { robotEnergy = 0 }

taskedRobot :: Robot
taskedRobot = fullEnergyRobot { robotTask = Just Explore }

untaskedRobot :: Robot
untaskedRobot = fullEnergyRobot { robotTask = Nothing }

readyRobot :: Robot
readyRobot = fullEnergyRobot { robotStatus = Idle }

disabledRobot :: Robot
disabledRobot = fullEnergyRobot { robotStatus = Disabled }


main :: IO()
main = hspec $ do
  describe "consumeEnergy" $ do
    it "accepts valid energy consumption" $ do
      consumeEnergy 10 fullEnergyRobot
        `shouldBe` Right (Robot 1 "Adam" 90 Idle (Just Explore))
    it "rejects negative amounts" $ do
      consumeEnergy (-10) fullEnergyRobot
        `shouldBe` Left InvalidEnergyAmount
    it "rejects if robot has insufficient energy" $ do
      consumeEnergy 101 fullEnergyRobot
        `shouldBe` Left InsufficientEnergy

  describe "rechargeEnergy" $ do
    it "accepts valid energy recharge" $ do
      rechargeEnergy (10) lowEnergyRobot
        `shouldBe` Right (Robot 1 "Adam" 100 Idle (Just Explore))
    it "rejects non-positive amounts" $ do
      rechargeEnergy (-1) lowEnergyRobot
        `shouldBe` Left InvalidEnergyAmount

  describe "taskAssignment" $ do
    describe "assignTask" $ do
      it "should assign a task successfully" $ do
        assignTask Explore untaskedRobot
          `shouldBe` taskedRobot

    describe "unassignTask" $ do
      it "should unassign a task successfully" $ do
        unassignTask taskedRobot
        `shouldBe` Right untaskedRobot

  describe "canExecuteTask" $ do
    it "should execute a task successfully" $ do
      canExecuteTask readyRobot
      `shouldBe` True
    it "should reject a disabled robot" $ do
      canExecuteTask disabledRobot
      `shouldBe` False
    it "should reject a robot with no energy" $ do
      canExecuteTask noEnergyRobot
      `shouldBe` False
    it "should reject an untasked robot" $ do
      canExecuteTask untaskedRobot
      `shouldBe` False

  describe "errorMessage" $ do
    it "formats invalid energy errors" $ do
      errorMessage InvalidEnergyAmount
        `shouldBe` "> Invalid energy amount!"

    it "formats insufficient energy errors" $ do
      errorMessage InsufficientEnergy
        `shouldBe` "> Insufficient energy!"