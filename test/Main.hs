module Main (main) where

import Test.Hspec
import Ashfall.Robot
import Ashfall.Error

main :: IO()
main = hspec $ do
  describe "consumeEnergy" $ do
    let fullEnergyRobot = Robot 1 "Adam" 100 Idle Explore
    it "accepts valid energy consumption" $ do
      consumeEnergy 10 fullEnergyRobot
        `shouldBe` Right (Robot 1 "Adam" 90 Idle Explore)
    it "rejects negative amounts" $ do
      consumeEnergy (-10) fullEnergyRobot
        `shouldBe` Left InvalidEnergyAmount
    it "rejects if robot has insufficient energy" $ do
      consumeEnergy 101 fullEnergyRobot
        `shouldBe` Left InsufficientEnergy

  describe "rechargeEnergy" $ do
    let lowEnergyRobot = Robot 1 "Adam" 90 Idle Explore
    it "accepts valid energy recharge" $ do
      rechargeEnergy (10) lowEnergyRobot
        `shouldBe` Right (Robot 1 "Adam" 100 Idle Explore)
    it "rejects non-positive amounts" $ do
      rechargeEnergy (-1) lowEnergyRobot
        `shouldBe` Left InvalidEnergyAmount

  describe "errorMessage" $ do
    it "formats invalid energy errors" $ do
      errorMessage InvalidEnergyAmount
        `shouldBe` "> Invalid energy amount!"

    it "formats insufficient energy errors" $ do
      errorMessage InsufficientEnergy
        `shouldBe` "> Insufficient energy!"