import FRP.Yampa (reactimate)
import Data.Yaml (decodeFileThrow)

import Types () -- We only require FromJSON instances
import Output (displayFunction)
import GameLogic (gameSF)
import Utils  ( loadDisplayResources
              , loadFramerateManager
              , initialInputs
              , detectDTimeAndInputs
              , loadBatons
              )

--

main = do

  gameConfigs       <- decodeFileThrow "configs/GameConfigs.yaml"
  displayConfigs    <- decodeFileThrow "configs/DisplayConfigs.yaml"
  displayResources  <- loadDisplayResources displayConfigs
  framerateManager  <- loadFramerateManager displayConfigs
  batons            <- loadBatons displayConfigs 0

  reactimate 
    (initialInputs framerateManager) 
    (detectDTimeAndInputs framerateManager)
    (displayFunction (displayConfigs, displayResources))
    (gameSF gameConfigs batons)