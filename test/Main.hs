module Main (main) where

import Test.Hspec
import Ashfall.Robot

main :: IO()
main = hspec $ do
  let robot = Robot 1 "Adam" 100 Idle

  describe "consumeEnergy" $ do
    it "rejects negative amounts" $ do
      consumeEnergy (-10) robot
        `shouldBe` Left "> Invalid energy amount!"
    it "rejects if robot has insufficient energy" $ do
      consumeEnergy (101) robot
        `shouldBe` Left "> Robot is out of energy!"

  describe "rechargeEnergy" $ do
    it "rejects non-positive amounts" $ do
      rechargeEnergy (-1) robot
        `shouldBe` Left "> Invalid recharge amount!"