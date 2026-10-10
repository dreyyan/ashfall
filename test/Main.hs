module Main (main) where

import Test.Hspec

import Ashfall.Base
import Ashfall.Error
import Ashfall.Resource
import Ashfall.Robot
import Ashfall.Storage
import Fixtures

import qualified Data.Map.Strict as Map

main :: IO()
main = hspec $ do
  describe "Robot" $ do
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

    describe "robotRemainingCapacity" $ do
      it "calculates remaining robot capacity" $ do
        robotRemainingCapacity readyRobot `shouldBe` 15

  describe "Base" $ do

    describe "calculateCost" $ do
      it "calculates scrap capacity cost" $ do
        calculateCost (Scrap, 10) `shouldBe` 10
      it "calculates iron capacity cost" $ do
        calculateCost (Iron, 10) `shouldBe` 20
      it "calculates crystal capacity cost" $ do
        calculateCost (Crystal, 10) `shouldBe` 30
      it "calculates fuel capacity cost" $ do
        calculateCost (Fuel, 10) `shouldBe` 20

    describe "storageUsed" $ do
      it "calculates total used storage capacity" $ do
        storageUsed (baseStorage richBase) `shouldBe` 100
      it "returns zero for empty storage" $ do
        storageUsed (Storage Map.empty) `shouldBe` 0

    describe "remainingCapacity" $ do
      it "calculates remaining base capacity" $ do
        remainingCapacity defaultBase `shouldBe` 100

    describe "canStore" $ do
      it "accepts resources within available capacity" $ do
        canStore Scrap 10 defaultBase
          `shouldBe` True
      it "accepts resources that exactly fill capacity" $ do
        canStore Scrap 100 defaultBase
          `shouldBe` True
      it "rejects resources exceeding capacity" $ do
        canStore Scrap 101 defaultBase
          `shouldBe` False
      it "rejects zero amounts" $ do
        canStore Scrap 0 defaultBase
          `shouldBe` False
      it "rejects resources when storage is already full" $ do
        canStore Scrap 1 richBase
          `shouldBe` False

  describe "Resource" $ do
    describe "resourceDescription" $ do
      it "returns the scrap description" $ do
        resourceDescription Scrap
          `shouldBe` "Salvaged metal and machinery parts."
      it "returns the iron description" $ do
        resourceDescription Iron
          `shouldBe` "A durable metal used for construction."
      it "returns the crystal description" $ do
        resourceDescription Crystal
          `shouldBe` "A rare crystalline material."
      it "returns the fuel description" $ do
        resourceDescription Fuel
          `shouldBe` "A combustible resource used to power machinery."

    describe "resourceCapacityCost" $ do
      it "returns scrap capacity cost" $ do
        resourceCapacityCost Scrap `shouldBe` 1
      it "returns iron capacity cost" $ do
        resourceCapacityCost Iron `shouldBe` 2
      it "returns crystal capacity cost" $ do
        resourceCapacityCost Crystal `shouldBe` 3
      it "returns fuel capacity cost" $ do
        resourceCapacityCost Fuel `shouldBe` 2

  describe "Storage" $ do
    describe "addToStorage" $ do
      it "adds scrap successfully" $ do
        addToStorage Scrap 10 emptyStorage
          `shouldBe` Right (Storage (Map.singleton Scrap 10))
      it "adds iron successfully" $ do
        addToStorage Iron 10 emptyStorage
          `shouldBe` Right (Storage (Map.singleton Iron 10))
      it "adds crystal successfully" $ do
        addToStorage Crystal 10 emptyStorage
          `shouldBe` Right (Storage (Map.singleton Crystal 10))
      it "adds fuel successfully" $ do
        addToStorage Fuel 10 emptyStorage
          `shouldBe` Right (Storage (Map.singleton Fuel 10))
      it "rejects zero amount" $ do
        addToStorage Scrap 0 emptyStorage
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        addToStorage Scrap (-1) emptyStorage
          `shouldBe` Left InvalidResourceAmount

    describe "removeFromStorage" $ do
      it "removes scrap successfully" $ do
        removeFromStorage Scrap 20 richStorage
          `shouldBe` Right
          (Storage (Map.adjust (subtract 20) Scrap (resources richStorage)))
      it "removes iron successfully" $ do
        removeFromStorage Iron 15 richStorage
          `shouldBe` Right
          (Storage (Map.adjust (subtract 15) Iron (resources richStorage)))
      it "removes crystal successfully" $ do
        removeFromStorage Crystal 10 richStorage
          `shouldBe` Right
          (Storage (Map.adjust (subtract 10) Crystal (resources richStorage)))
      it "removes fuel successfully" $ do
        removeFromStorage Fuel 10 richStorage
          `shouldBe` Right
          (Storage (Map.adjust (subtract 10) Fuel (resources richStorage)))
      it "rejects zero amount" $ do
        removeFromStorage Fuel 0 richStorage
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        removeFromStorage Fuel (-1) richStorage
          `shouldBe` Left InvalidResourceAmount

  describe "Error" $ do
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