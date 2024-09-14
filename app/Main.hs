import Data.Yaml (decodeFileThrow)
import FRP.Yampa (reactimate)
import GameLogic (gameSF)
import Output (displayFunction)
import Types (DisplayResources (window)) -- plus FromJSON instances

import qualified SDL
import qualified Utils


main :: IO ()
main = do

  gameConfigs <- decodeFileThrow "configs/GameConfigs.yaml"
  displayConfigs <- decodeFileThrow "configs/DisplayConfigs.yaml"
  displayResources <- Utils.loadDisplayResources displayConfigs
  framerateManager <- Utils.loadFramerateManager displayConfigs
  batons <- Utils.loadBatons displayConfigs 0

  let
    initAction = Utils.initialInputs framerateManager
    inputSensing = Utils.detectDTimeAndInputs framerateManager
    outputProcessing = displayFunction displayConfigs displayResources
    signalFunction = gameSF gameConfigs batons

  reactimate initAction inputSensing outputProcessing signalFunction
  
  SDL.destroyWindow $ window displayResources
  SDL.quit
