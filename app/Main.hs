import Data.Yaml (decodeFileThrow)
import FRP.Yampa (reactimate)
import Game (gameSF)
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

  let
    initAction = Utils.initialInputs framerateManager
    inputSensing = Utils.detectDTimeAndInputs framerateManager
    outputProcessing = displayFunction displayConfigs displayResources
    randSeed = 0
    baton = Utils.initialBaton displayConfigs randSeed
    signalFunction = gameSF gameConfigs baton

  reactimate initAction inputSensing outputProcessing signalFunction
  
  SDL.destroyWindow $ window displayResources
  SDL.quit
