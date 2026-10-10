module Main (main) where

import Test.Hspec

import Ashfall.Base
import Ashfall.Error
import Ashfall.Resource
import Ashfall.Robot
import Ashfall.Storage
import Fixtures

import qualified Data.Map.Strict as Map
import Ashfall.Transfer (depositResources)
import Ashfall.Constants (robotDefaultCapacity)
import Ashfall.Location (resourceWeights, LocationType (Scrapyard, IronQuarry, CrystalCavern, GasStation), getResourceChance, Location (locationResources), scrapyard, availableResources, canFindResource, totalResourceChance, validateLocation)

main :: IO()
main = hspec $ do
  describe "Base" $ do
    describe "addResources" $ do
      it "adds scrap successfully" $ do
        addResources defaultBase Scrap 10
          `shouldBe` Right (defaultBase
            { baseStorage = Storage (Map.singleton Scrap 10) })
      it "adds iron successfully" $ do
        addResources defaultBase Iron 10
          `shouldBe` Right (defaultBase
            { baseStorage = Storage (Map.singleton Iron 10) })
      it "adds crystal successfully" $ do
        addResources defaultBase Crystal 10
          `shouldBe` Right (defaultBase
            { baseStorage = Storage (Map.singleton Crystal 10) })
      it "adds fuel successfully" $ do
        addResources defaultBase Fuel 10
          `shouldBe` Right (defaultBase
            { baseStorage = Storage (Map.singleton Fuel 10) })
      it "rejects zero amount" $ do
        addResources defaultBase Scrap 0
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        addResources defaultBase Scrap (-1)
          `shouldBe` Left InvalidResourceAmount
      it "rejects resources exceeding capacity" $ do
        addResources defaultBase Scrap 101
          `shouldBe` Left InsufficientCapacity
      it "accepts resources that exactly fill capacity" $ do
        addResources defaultBase Scrap 100
          `shouldBe` Right (defaultBase
            { baseStorage = Storage (Map.singleton Scrap 100) })
      it "rejects resources when storage is full" $ do
        addResources richBase Scrap 1
          `shouldBe` Left InsufficientCapacity

    describe "consumeResources" $ do
      it "removes scrap successfully" $ do
        consumeResources richBase Scrap 20
          `shouldBe` Right (richBase
            { baseStorage = Storage
                (Map.adjust (subtract 20) Scrap (resources (baseStorage richBase))) })
      it "removes iron successfully" $ do
        consumeResources richBase Iron 15
          `shouldBe` Right (richBase
            { baseStorage = Storage
                (Map.adjust (subtract 15) Iron (resources (baseStorage richBase))) })
      it "removes crystal successfully" $ do
        consumeResources richBase Crystal 10
          `shouldBe` Right (richBase
            { baseStorage = Storage
                (Map.adjust (subtract 10) Crystal (resources (baseStorage richBase))) })
      it "removes fuel successfully" $ do
        consumeResources richBase Fuel 10
          `shouldBe` Right (richBase
            { baseStorage = Storage
                (Map.adjust (subtract 10) Fuel (resources (baseStorage richBase))) })
      it "rejects zero amount" $ do
        consumeResources richBase Fuel 0
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        consumeResources richBase Fuel (-1)
          `shouldBe` Left InvalidResourceAmount
      it "rejects insufficient resources" $ do
        consumeResources richBase Scrap 21
          `shouldBe` Left InsufficientResources
      it "accepts consuming all available resources" $ do
        consumeResources richBase Scrap 20
          `shouldBe` Right (richBase
            { baseStorage = Storage
                (Map.insert Scrap 0 (resources (baseStorage richBase))) })

    describe "remainingCapacity" $ do
      it "calculates remaining base capacity" $ do
        remainingCapacity defaultBase `shouldBe` 100

  describe "Location" $ do
    describe "resourceWeights" $ do
      it "returns correct weights for Scrapyard" $ do
        resourceWeights Scrapyard
          `shouldBe` [(Scrap, 75), (Iron, 20), (Fuel, 5)]
      it "returns correct weights for IronQuarry" $ do
        resourceWeights IronQuarry
          `shouldBe` [(Scrap, 20), (Iron, 75), (Crystal, 5)]
      it "returns correct weights for CrystalCavern" $ do
        resourceWeights CrystalCavern
          `shouldBe` [(Scrap, 10), (Iron, 30), (Crystal, 60)]
      it "returns correct weights for GasStation" $ do
        resourceWeights GasStation
          `shouldBe` [(Scrap, 20), (Iron, 10), (Fuel, 70)]

  describe "getResourceChance" $ do
    it "returns the correct probability for Scrap" $ do
      getResourceChance Scrap scrapyard
        `shouldBe` 75
    it "returns the correct probability for Iron" $ do
      getResourceChance Iron scrapyard
        `shouldBe` 20
    it "returns the correct probability for Fuel" $ do
      getResourceChance Fuel scrapyard
        `shouldBe` 5
    it "returns 0 for a resource not found at the location" $ do
      getResourceChance Crystal scrapyard
        `shouldBe` 0
    it "returns 0 for a location with no resources" $ do
      getResourceChance Scrap (scrapyard { locationResources = Map.empty })
        `shouldBe` 0

  describe "availableResources" $ do
    it "returns all available resources at a location" $ do
      availableResources scrapyard
        `shouldMatchList` [Scrap, Iron, Fuel]
    it "returns an empty list when no resources exist" $ do
      availableResources (scrapyard { locationResources = Map.empty })
        `shouldBe` []
    it "excludes resources with a 0% probability" $ do
      availableResources
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 75)
                , (Iron, 25)
                , (Crystal, 0)
                ]
          })
        `shouldMatchList` [Scrap, Iron]

  describe "canFindResource" $ do
    it "returns True when a resource is available" $ do
      canFindResource Scrap scrapyard
        `shouldBe` True
      canFindResource Iron scrapyard
        `shouldBe` True
      canFindResource Fuel scrapyard
        `shouldBe` True
    it "returns False when a resource is unavailable" $ do
      canFindResource Crystal scrapyard
        `shouldBe` False
    it "returns False when a resource has a 0% probability" $ do
      canFindResource Crystal
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 75)
                , (Iron, 25)
                , (Crystal, 0)
                ]
          })
        `shouldBe` False
    it "returns False when the location has no resources" $ do
      canFindResource Scrap
        (scrapyard { locationResources = Map.empty })
        `shouldBe` False

  describe "totalResourceChance" $ do
    it "returns 100 for a valid location" $ do
      totalResourceChance scrapyard
        `shouldBe` 100
    it "returns 0 for an empty resource map" $ do
      totalResourceChance
        (scrapyard { locationResources = Map.empty })
        `shouldBe` 0
    it "returns the total even if probabilities do not sum to 100" $ do
      totalResourceChance
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 40)
                , (Iron, 30)
                , (Fuel, 10)
                ]
          })
        `shouldBe` 80
    it "sums probabilities even when some are negative" $ do
      totalResourceChance
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 80)
                , (Iron, -10)
                ]
          })
        `shouldBe` 70

  describe "validateLocation" $ do
    it "accepts a valid location" $ do
      validateLocation scrapyard
        `shouldBe` True
    it "accepts a resource with exactly 100% probability" $ do
      validateLocation
        (scrapyard
          { locationResources =
              Map.singleton Scrap 100
          })
        `shouldBe` True
    it "accepts resources with 0% probability" $ do
      validateLocation
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 75)
                , (Iron, 25)
                , (Crystal, 0)
                ]
          })
        `shouldBe` True
    it "rejects probabilities totaling less than 100" $ do
      validateLocation
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 50)
                , (Iron, 30)
                ]
          })
        `shouldBe` False
    it "rejects probabilities totaling more than 100" $ do
      validateLocation
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 80)
                , (Iron, 40)
                ]
          })
        `shouldBe` False
    it "rejects negative probabilities" $ do
      validateLocation
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 110)
                , (Iron, -10)
                ]
          })
        `shouldBe` False
    it "rejects individual probabilities exceeding 100" $ do
      validateLocation
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 120)
                , (Iron, -20)
                ]
          })
        `shouldBe` False
    it "rejects an empty resource map" $ do
      validateLocation
        (scrapyard { locationResources = Map.empty })
        `shouldBe` False
    it "rejects locations containing only zero probabilities" $ do
      validateLocation
        (scrapyard
          { locationResources =
              Map.fromList
                [ (Scrap, 0)
                , (Iron, 0)
                ]
          })
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

    describe "canPerformTask" $ do
      it "allows a working robot with an assigned task" $ do
        canPerformTask workingRobot
          `shouldBe` Right ()
      it "rejects an idle robot" $ do
        canPerformTask readyRobot
          `shouldBe` Left NotWorking
      it "rejects a disabled robot" $ do
        canPerformTask disabledRobot
          `shouldBe` Left DisabledRobot
      it "rejects a working robot without an assigned task" $ do
        canPerformTask (untaskedRobot { robotStatus = Working })
          `shouldBe` Left NoTaskAssigned
      it "allows a working robot with no energy to attempt a task" $ do
        canPerformTask (noEnergyRobot { robotStatus = Working })
          `shouldBe` Right ()

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

    describe "addToInventory" $ do
      it "adds scrap successfully" $ do
        addToInventory readyRobot Scrap 10
          `shouldBe` Right (readyRobot
            { robotStorage = Storage (Map.singleton Scrap 10) })
      it "adds iron successfully" $ do
        addToInventory readyRobot Iron 7
          `shouldBe` Right (readyRobot
            { robotStorage = Storage (Map.singleton Iron 7) })
      it "adds crystal successfully" $ do
        addToInventory readyRobot Crystal 5
          `shouldBe` Right (readyRobot
            { robotStorage = Storage (Map.singleton Crystal 5) })
      it "adds fuel successfully" $ do
        addToInventory readyRobot Fuel 7
          `shouldBe` Right (readyRobot
            { robotStorage = Storage (Map.singleton Fuel 7) })
      it "rejects zero amount" $ do
        addToInventory readyRobot Scrap 0
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        addToInventory readyRobot Scrap (-1)
          `shouldBe` Left InvalidResourceAmount
      it "rejects resources exceeding capacity" $ do
        addToInventory readyRobot Scrap 21
          `shouldBe` Left InsufficientCapacity
      it "accepts resources that exactly fill capacity" $ do
        addToInventory readyRobot Scrap 15
          `shouldBe` Right (readyRobot
            { robotStorage = Storage (Map.singleton Scrap 15) })
      it "rejects resources when inventory is full" $ do
        addToInventory fullRobot Scrap 1
          `shouldBe` Left InsufficientCapacity
      it "adds resources to an existing inventory" $ do
        addToInventory loadedRobot Scrap 3
          `shouldBe` Right (loadedRobot
            { robotStorage = Storage
                (Map.insert Scrap 8 (resources (robotStorage loadedRobot))) })

    describe "removeFromInventory" $ do
      it "removes scrap successfully" $ do
        removeFromInventory loadedRobot Scrap 5
          `shouldBe` Right (loadedRobot
            { robotStorage = Storage
                (Map.insert Scrap 0 (resources (robotStorage loadedRobot))) })
      it "removes iron successfully" $ do
        removeFromInventory loadedRobot Iron 2
          `shouldBe` Right (loadedRobot
            { robotStorage = Storage
                (Map.insert Iron 0 (resources (robotStorage loadedRobot))) })
      it "removes crystal successfully" $ do
        removeFromInventory loadedRobot Crystal 1
          `shouldBe` Right (loadedRobot
            { robotStorage = Storage
                (Map.insert Crystal 0 (resources (robotStorage loadedRobot))) })
      it "removes fuel successfully" $ do
        removeFromInventory fullRobot Fuel 10
          `shouldBe` Left InsufficientResources
      it "rejects zero amount" $ do
        removeFromInventory loadedRobot Scrap 0
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        removeFromInventory loadedRobot Scrap (-1)
          `shouldBe` Left InvalidResourceAmount
      it "rejects insufficient resources" $ do
        removeFromInventory loadedRobot Scrap 6
          `shouldBe` Left InsufficientResources
      it "rejects removal from empty inventory" $ do
        removeFromInventory readyRobot Scrap 1
          `shouldBe` Left InsufficientResources
      it "accepts removing part of an existing resource" $ do
        removeFromInventory loadedRobot Scrap 2
          `shouldBe` Right (loadedRobot
            { robotStorage = Storage
                (Map.insert Scrap 3 (resources (robotStorage loadedRobot))) })

    describe "mineResources" $ do
      it "mines resources successfully" $ do
        mineResources readyRobot
          `shouldBe` Right (readyRobot {
            robotEnergy = 90,
            robotStorage = Storage (Map.singleton Scrap 1)
          })
      it "rejects mining with insufficient energy" $ do
        mineResources noEnergyRobot
          `shouldBe` Left InsufficientEnergy
      it "rejects mining when inventory is full" $ do
        mineResources fullRobot
          `shouldBe` Left InsufficientCapacity
      it "accepts mining at exact energy and capacity limits" $ do
        mineResources exactLimitRobot
          `shouldBe` Right
            (exactLimitRobot
              { robotEnergy = 0
              , robotStorage =
                  Storage (Map.singleton Scrap robotDefaultCapacity)
              })

  describe "Storage" $ do
    describe "addToStorage" $ do
      it "adds scrap successfully" $ do
        addToStorage emptyStorage Scrap 10
          `shouldBe` Right (Storage (Map.singleton Scrap 10))
      it "adds iron successfully" $ do
        addToStorage emptyStorage Iron 10
          `shouldBe` Right (Storage (Map.singleton Iron 10))
      it "adds crystal successfully" $ do
        addToStorage emptyStorage Crystal 10
          `shouldBe` Right (Storage (Map.singleton Crystal 10))
      it "adds fuel successfully" $ do
        addToStorage emptyStorage Fuel 10
          `shouldBe` Right (Storage (Map.singleton Fuel 10))
      it "rejects zero amount" $ do
        addToStorage emptyStorage Scrap 0
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        addToStorage emptyStorage Scrap (-1)
          `shouldBe` Left InvalidResourceAmount

    describe "removeFromStorage" $ do
      it "removes scrap successfully" $ do
        removeFromStorage richStorage Scrap 20
          `shouldBe` Right
          (Storage (Map.adjust (subtract 20) Scrap (resources richStorage)))
      it "removes iron successfully" $ do
        removeFromStorage richStorage Iron 15
          `shouldBe` Right
          (Storage (Map.adjust (subtract 15) Iron (resources richStorage)))
      it "removes crystal successfully" $ do
        removeFromStorage richStorage Crystal 10
          `shouldBe` Right
          (Storage (Map.adjust (subtract 10) Crystal (resources richStorage)))
      it "removes fuel successfully" $ do
        removeFromStorage richStorage Fuel 10
          `shouldBe` Right
          (Storage (Map.adjust (subtract 10) Fuel (resources richStorage)))
      it "rejects zero amount" $ do
        removeFromStorage richStorage Fuel 0
          `shouldBe` Left InvalidResourceAmount
      it "rejects negative amount" $ do
        removeFromStorage richStorage Fuel (-1)
          `shouldBe` Left InvalidResourceAmount

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

    describe "canStore" $ do
      it "accepts resources within available capacity" $ do
        canStore Scrap 10 100 emptyStorage
          `shouldBe` True
      it "accepts resources that exactly fill capacity" $ do
        canStore Scrap 100 100 emptyStorage
          `shouldBe` True
      it "rejects resources exceeding capacity" $ do
        canStore Scrap 101 100 emptyStorage
          `shouldBe` False
      it "rejects zero amounts" $ do
        canStore Scrap 0 100 emptyStorage
          `shouldBe` False
      it "rejects negative amounts" $ do
        canStore Scrap (-1) 100 emptyStorage
          `shouldBe` False
      it "rejects resources when storage is already full" $ do
        canStore Scrap 1 100 richStorage
          `shouldBe` False
      it "accepts resources within partially occupied storage" $ do
        canStore Scrap 10 100 smallStorage
          `shouldBe` True

  describe "Transfer" $ do
    describe "depositResources" $ do
      it "transfers resources from robot to base" $ do
        depositResources loadedRobot Scrap 3 defaultBase
          `shouldBe` Right
            ( loadedRobot
                { robotStorage = Storage
                    (Map.insert Scrap 2 (resources (robotStorage loadedRobot)))
                }
            , defaultBase
                { baseStorage = Storage (Map.singleton Scrap 3)
                }
            )
      it "allows depositing all available resources" $ do
        depositResources loadedRobot Scrap 5 defaultBase
          `shouldBe` Right
            ( loadedRobot
                { robotStorage = Storage
                    (Map.insert Scrap 0 (resources (robotStorage loadedRobot)))
                }
            , defaultBase
                { baseStorage = Storage (Map.singleton Scrap 5)
                }
            )
      it "rejects deposits when robot has insufficient resources" $ do
        depositResources loadedRobot Scrap 10 defaultBase
          `shouldBe` Left InsufficientResources
      it "rejects deposits when base has insufficient capacity" $ do
        depositResources loadedRobot Scrap 5 richBase
          `shouldBe` Left InsufficientCapacity
      it "rejects zero amounts" $ do
        depositResources loadedRobot Scrap 0 defaultBase
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