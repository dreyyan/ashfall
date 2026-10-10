module Main (main) where

import Test.Hspec

import Ashfall.Base
import Ashfall.Constants (robotDefaultCapacity)
import Ashfall.Equipment
import Ashfall.Error
import Ashfall.Location (resourceWeights, LocationType (Scrapyard, IronQuarry, CrystalCavern, AbandonedRefinery), getResourceChance, Location (locationResources), scrapyard, availableResources, canFindResource, totalResourceChance, validateLocation)
import Ashfall.Resource
import Ashfall.Robot
import Ashfall.Storage
import Ashfall.Transfer (depositResources)
import Fixtures

import qualified Data.Map.Strict as Map
import Ashfall.Mining (selectResource, calculateMiningYield, validateMining, mineResources)

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

  describe "Equipment" $ do
    describe "equipmentEnergyCost" $ do
      it "returns 1 for BasicDrill" $ do
        equipmentEnergyCost BasicDrill
          `shouldBe` 1
      it "returns 2 for IndustrialDrill" $ do
        equipmentEnergyCost IndustrialDrill
          `shouldBe` 2
      it "returns 3 for AdvancedExtractor" $ do
        equipmentEnergyCost AdvancedExtractor
          `shouldBe` 3

    describe "equipmentYield" $ do
      it "returns (1, 1) for BasicDrill" $ do
        equipmentYield BasicDrill
          `shouldBe` (1, 1)
      it "returns (2, 4) for IndustrialDrill" $ do
        equipmentYield IndustrialDrill
          `shouldBe` (2, 4)
      it "returns (4, 7) for AdvancedExtractor" $ do
        equipmentYield AdvancedExtractor
          `shouldBe` (4, 7)

    describe "equipmentMineableResources" $ do
      it "returns Scrap for BasicDrill" $ do
        equipmentMineableResources BasicDrill
          `shouldMatchList` [Scrap]
      it "returns Scrap, Iron, and Fuel for IndustrialDrill" $ do
        equipmentMineableResources IndustrialDrill
          `shouldMatchList` [Scrap, Iron, Fuel]
      it "returns all resources for AdvancedExtractor" $ do
        equipmentMineableResources AdvancedExtractor
          `shouldMatchList` [Scrap, Iron, Crystal, Fuel]

    describe "canMineResource" $ do
      it "allows BasicDrill to mine Scrap" $ do
        canMineResource Scrap BasicDrill
          `shouldBe` True
      it "rejects unsupported resources for BasicDrill" $ do
        canMineResource Iron BasicDrill
          `shouldBe` False
        canMineResource Crystal BasicDrill
          `shouldBe` False
        canMineResource Fuel BasicDrill
          `shouldBe` False
      it "allows IndustrialDrill to mine supported resources" $ do
        canMineResource Scrap IndustrialDrill
          `shouldBe` True
        canMineResource Iron IndustrialDrill
          `shouldBe` True
        canMineResource Fuel IndustrialDrill
          `shouldBe` True
      it "rejects Crystal for IndustrialDrill" $ do
        canMineResource Crystal IndustrialDrill
          `shouldBe` False
      it "allows AdvancedExtractor to mine all resources" $ do
        canMineResource Scrap AdvancedExtractor
          `shouldBe` True
        canMineResource Iron AdvancedExtractor
          `shouldBe` True
        canMineResource Crystal AdvancedExtractor
          `shouldBe` True
        canMineResource Fuel AdvancedExtractor
          `shouldBe` True

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
      it "returns correct weights for AbandonedRefinery" $ do
        resourceWeights AbandonedRefinery
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

  describe "Mining" $ do
    describe "selectResource" $ do
      it "selects Scrap at the minimum roll" $
        selectResource scrapyard 1 `shouldBe` Just Scrap
      it "selects Scrap within its probability range" $
        selectResource scrapyard 50 `shouldBe` Just Scrap
      it "selects Scrap at its upper boundary" $
        selectResource scrapyard 75 `shouldBe` Just Scrap
      it "selects Iron at its lower boundary" $
        selectResource scrapyard 76 `shouldBe` Just Iron
      it "selects Iron within its probability range" $
        selectResource scrapyard 85 `shouldBe` Just Iron
      it "selects Iron at its upper boundary" $
        selectResource scrapyard 95 `shouldBe` Just Iron
      it "selects Fuel at its lower boundary" $
        selectResource scrapyard 96 `shouldBe` Just Fuel
      it "selects Fuel at the maximum roll" $
        selectResource scrapyard 100 `shouldBe` Just Fuel
      it "rejects a zero roll" $
        selectResource scrapyard 0 `shouldBe` Nothing
      it "rejects a negative roll" $
        selectResource scrapyard (-1) `shouldBe` Nothing
      it "rejects a roll above 100" $
        selectResource scrapyard 101 `shouldBe` Nothing
      it "rejects a location with probabilities totaling less than 100" $
        selectResource (scrapyard {locationResources = Map.fromList [(Scrap, 50), (Iron, 20)]}) 25 `shouldBe` Nothing
      it "rejects a location with probabilities totaling more than 100" $
        selectResource (scrapyard {locationResources = Map.fromList [(Scrap, 80), (Iron, 30)]}) 25 `shouldBe` Nothing
      it "rejects a location with negative probabilities" $
        selectResource (scrapyard {locationResources = Map.fromList [(Scrap, 110), (Iron, -10)]}) 25 `shouldBe` Nothing
      it "rejects a location with no resources" $
        selectResource (scrapyard {locationResources = Map.empty}) 25 `shouldBe` Nothing

    describe "calculateMiningYield" $ do
      it "returns 1 for BasicDrill with roll 1" $
        calculateMiningYield BasicDrill 1 `shouldBe` 1
      it "returns 1 for BasicDrill with roll 5" $
        calculateMiningYield BasicDrill 5 `shouldBe` 1
      it "returns minimum yield for IndustrialDrill" $
        calculateMiningYield IndustrialDrill 1 `shouldBe` 2
      it "returns middle yield for IndustrialDrill" $
        calculateMiningYield IndustrialDrill 2 `shouldBe` 3
      it "returns maximum yield for IndustrialDrill" $
        calculateMiningYield IndustrialDrill 3 `shouldBe` 4
      it "wraps IndustrialDrill yield back to minimum" $
        calculateMiningYield IndustrialDrill 4 `shouldBe` 2
      it "returns minimum yield for AdvancedExtractor" $
        calculateMiningYield AdvancedExtractor 1 `shouldBe` 4
      it "returns second yield for AdvancedExtractor" $
        calculateMiningYield AdvancedExtractor 2 `shouldBe` 5
      it "returns third yield for AdvancedExtractor" $
        calculateMiningYield AdvancedExtractor 3 `shouldBe` 6
      it "returns maximum yield for AdvancedExtractor" $
        calculateMiningYield AdvancedExtractor 4 `shouldBe` 7
      it "wraps AdvancedExtractor yield back to minimum" $
        calculateMiningYield AdvancedExtractor 5 `shouldBe` 4

    describe "validateMining" $ do
      let miningRobot = readyRobot
            { robotStatus = Working
            , robotTask = Just Mine
            , robotEnergy = 100
            , robotEquipment = AdvancedExtractor
            }
      it "allows a robot with valid equipment to mine Scrap" $
        validateMining miningRobot scrapyard Scrap `shouldBe` Right ()
      it "allows a robot with valid equipment to mine Iron" $
        validateMining miningRobot scrapyard Iron `shouldBe` Right ()
      it "rejects a disabled robot" $
        validateMining (miningRobot {robotStatus = Disabled}) scrapyard Scrap `shouldBe` Left DisabledRobot
      it "rejects a robot without an assigned task" $
        validateMining (miningRobot {robotTask = Nothing}) scrapyard Scrap `shouldBe` Left NoTaskAssigned
      it "rejects a robot assigned to Explore" $
        validateMining (miningRobot {robotTask = Just Explore}) scrapyard Scrap `shouldBe` Left NoTaskAssigned
      it "rejects a robot assigned to Recharge" $
        validateMining (miningRobot {robotTask = Just Recharge}) scrapyard Scrap `shouldBe` Left NoTaskAssigned
      it "rejects a resource unavailable at the location" $
        validateMining miningRobot scrapyard Crystal `shouldBe` Left ResourceUnavailable
      it "rejects mining Iron with BasicDrill" $
        validateMining (miningRobot {robotEquipment = BasicDrill}) scrapyard Iron `shouldBe` Left UnsupportedResource
      it "rejects mining Fuel with BasicDrill" $
        validateMining (miningRobot {robotEquipment = BasicDrill}) scrapyard Fuel `shouldBe` Left UnsupportedResource
      it "rejects mining Crystal with IndustrialDrill at a crystal-bearing location" $
        validateMining (miningRobot {robotEquipment = IndustrialDrill}) (scrapyard {locationResources = Map.fromList [(Crystal, 100)]}) Crystal `shouldBe` Left UnsupportedResource
      it "rejects a robot with insufficient energy" $
        validateMining (miningRobot {robotEnergy = 2}) scrapyard Scrap `shouldBe` Left InsufficientEnergy
      it "allows mining when energy exactly equals equipment cost" $
        validateMining (miningRobot {robotEnergy = 3}) scrapyard Scrap `shouldBe` Right ()
      it "allows BasicDrill to mine Scrap with exactly 1 energy" $
        validateMining (miningRobot {robotEquipment = BasicDrill, robotEnergy = 1}) scrapyard Scrap `shouldBe` Right ()
    
    describe "mineResources" $ do
      let miningRobot = readyRobot
            { robotStatus = Working
            , robotTask = Just Mine
            , robotEnergy = 100
            , robotEquipment = AdvancedExtractor
            }
      it "mines Scrap successfully" $ do
        let result = mineResources scrapyard 50 1 miningRobot
        fmap robotEnergy result `shouldBe` Right 97
        fmap (\r -> getResourceAmount Scrap (robotStorage r)) result `shouldBe` Right 4
      it "mines Iron successfully" $ do
        let result = mineResources scrapyard 85 2 miningRobot
        fmap robotEnergy result `shouldBe` Right 97
        fmap (\r -> getResourceAmount Iron (robotStorage r)) result `shouldBe` Right 5
      it "mines Fuel successfully" $ do
        let result = mineResources scrapyard 99 3 miningRobot
        fmap robotEnergy result `shouldBe` Right 97
        fmap (\r -> getResourceAmount Fuel (robotStorage r)) result `shouldBe` Right 6
      it "mines Scrap with BasicDrill" $ do
        let robot = miningRobot {robotEquipment = BasicDrill}
            result = mineResources scrapyard 50 1 robot
        fmap robotEnergy result `shouldBe` Right 99
        fmap (\r -> getResourceAmount Scrap (robotStorage r)) result `shouldBe` Right 1
      it "mines Iron with IndustrialDrill" $ do
        let robot = miningRobot {robotEquipment = IndustrialDrill}
            result = mineResources scrapyard 85 2 robot
        fmap robotEnergy result `shouldBe` Right 98
        fmap (\r -> getResourceAmount Iron (robotStorage r)) result `shouldBe` Right 3
      it "rejects a zero resource roll" $
        mineResources scrapyard 0 1 miningRobot `shouldBe` Left ResourceUnavailable
      it "rejects a resource roll above 100" $
        mineResources scrapyard 101 1 miningRobot `shouldBe` Left ResourceUnavailable
      it "rejects mining with a disabled robot" $
        mineResources scrapyard 50 1 (miningRobot {robotStatus = Disabled}) `shouldBe` Left DisabledRobot
      it "rejects mining without an assigned task" $
        mineResources scrapyard 50 1 (miningRobot {robotTask = Nothing}) `shouldBe` Left NoTaskAssigned
      it "rejects mining when assigned to Explore" $
        mineResources scrapyard 50 1 (miningRobot {robotTask = Just Explore}) `shouldBe` Left NoTaskAssigned
      it "rejects mining unsupported resources" $
        mineResources scrapyard 85 1 (miningRobot {robotEquipment = BasicDrill}) `shouldBe` Left UnsupportedResource
      it "rejects mining with insufficient energy" $
        mineResources scrapyard 50 1 (miningRobot {robotEnergy = 2}) `shouldBe` Left InsufficientEnergy
      it "allows mining with exactly enough energy" $ do
        let robot = miningRobot {robotEnergy = 3}
            result = mineResources scrapyard 50 1 robot
        fmap robotEnergy result `shouldBe` Right 0
      it "rejects mining Crystal at the Scrapyard" $
        validateMining miningRobot scrapyard Crystal `shouldBe` Left ResourceUnavailable
      it "rejects a location with invalid probabilities" $
        mineResources
          (scrapyard {locationResources = Map.fromList [(Scrap, 50), (Iron, 20)]})
          50 1 miningRobot `shouldBe` Left ResourceUnavailable

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
      let miningRobot = readyRobot
            { robotStatus = Working
            , robotTask = Just Mine
            , robotEnergy = 100
            , robotEquipment = AdvancedExtractor
            , robotStorage = Storage Map.empty
            }
      it "mines Scrap successfully" $ do
        let result = mineResources scrapyard 50 1 miningRobot
        fmap robotEnergy result `shouldBe` Right 97
        fmap (getResourceAmount Scrap . robotStorage) result `shouldBe` Right 4
      it "rejects mining with insufficient energy" $
        mineResources scrapyard 50 1 (miningRobot {robotEnergy = 2})
          `shouldBe` Left InsufficientEnergy
      it "rejects mining when inventory is full" $
        mineResources scrapyard 50 1
          (miningRobot {robotStorage = Storage (Map.singleton Scrap robotDefaultCapacity)})
          `shouldBe` Left InsufficientCapacity
      it "accepts mining with exactly enough energy" $ do
        let result = mineResources scrapyard 50 1 (miningRobot {robotEnergy = 3})
        fmap robotEnergy result `shouldBe` Right 0
      it "rejects mining with a disabled robot" $
        mineResources scrapyard 50 1 (miningRobot {robotStatus = Disabled})
          `shouldBe` Left DisabledRobot
      it "rejects mining an unsupported resource" $
        mineResources scrapyard 85 1 (miningRobot {robotEquipment = BasicDrill})
          `shouldBe` Left UnsupportedResource

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

    describe "getResourceAmount" $ do
      let storage = Storage (Map.fromList [(Scrap, 10), (Iron, 5), (Crystal, 2), (Fuel, 3)])
      it "returns the correct Scrap quantity" $
        getResourceAmount Scrap storage `shouldBe` 10
      it "returns the correct Iron quantity" $
        getResourceAmount Iron storage `shouldBe` 5
      it "returns the correct Crystal quantity" $
        getResourceAmount Crystal storage `shouldBe` 2
      it "returns the correct Fuel quantity" $
        getResourceAmount Fuel storage `shouldBe` 3
      it "returns zero when a resource is not stored" $
        getResourceAmount Crystal (Storage (Map.fromList [(Scrap, 10)])) `shouldBe` 0
      it "returns zero for empty storage" $
        getResourceAmount Scrap (Storage Map.empty) `shouldBe` 0
      it "returns zero for a resource explicitly stored with quantity zero" $
        getResourceAmount Iron (Storage (Map.fromList [(Iron, 0)])) `shouldBe` 0

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