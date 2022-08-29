import Types      ( DisplayResources (window) ) -- Plus FromJSON instances
import Output     ( displayFunction )
import GameLogic  ( gameSF )
import Utils      ( loadDisplayResources
                  , loadFramerateManager
                  , initialInputs
                  , detectDTimeAndInputs
                  , loadBatons
                  )

import FRP.Yampa  ( reactimate )
import Data.Yaml  ( decodeFileThrow )

import qualified SDL

--

main = do

  gameConfigs <- decodeFileThrow "configs/GameConfigs.yaml"
  displayConfigs <- decodeFileThrow "configs/DisplayConfigs.yaml"
  displayResources <- loadDisplayResources displayConfigs
  framerateManager <- loadFramerateManager displayConfigs
  batons <- loadBatons displayConfigs 0

  let
    initAction = initialInputs framerateManager
    inputSensing = detectDTimeAndInputs framerateManager
    outputProcessing = displayFunction (displayConfigs, displayResources)
    signalFunction = gameSF gameConfigs batons

  reactimate initAction inputSensing outputProcessing signalFunction
  
  SDL.destroyWindow $ window displayResources
  SDL.quit