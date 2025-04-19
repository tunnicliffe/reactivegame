{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module GameLogic
  ( startMenuSF
  , introSF
  , playingSF
  , winScreenSF
  , loseScreenSF
  , endScreenSF
  , pauseMenuSF
  ) where

import ClassyPrelude
import FRP.Yampa
import Types
import Types.Configs

import Data.Int (Int32)
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Foreign.C.Types (CInt)
import SDL (V2(..), V3(..), V4(..), Point(..), MouseButton(..))
import Stochastic (splitN, brownianMotion2D, brownianMotion3D)

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Input as IN
import qualified LifeHash as LH


pauseMenuSF :: PauseMenuConfigs -> Baton -> SF UserInputs GameOutputs
pauseMenuSF PauseMenuConfigs{..} _b = proc userI -> do
  let i = IN.keyRemapToHM userI pauseMenuInputMap
      pauseMenuColOut = pauseMenuCol
      quitPM = Quit `IN.pressBool` i
  unpauseEvent <- dropEvents 1 <<< edgeTag MSUnpauseLevel -< Unpause `IN.releaseBool` i

  returnA -< PauseMenuOutputs $ PauseMenuOutputsData{..}

---

startMenuSF :: StartMenuConfigs -> Baton -> SF UserInputs GameOutputs
startMenuSF StartMenuConfigs{..} b = proc userI -> do
  let i = IN.keyRemapToHM userI startMenuInputMap
      ballColOut = ballCol
      quitSM = Quit `IN.pressBool` i
  bm3D <- brownianMotion3D (v4ToV3 startMenuCol) 0.05 g1 g2 g3 -< ()
  bm2D <- brownianMotion2D (V2 100 100) 2.0 g4 g5 -< ()
  startMenuColOut <- arrPrim (fmap (fromInteger . round)) <<< arrPrim (fmap (reflected (0, 255))) -< v3ToV4 bm3D 255
  ballPos <- arrPrim (fmap round) <<< arrPrim (fmap (reflected (0,400))) -< bm2D 
  startGameEvent <- edgeTag (MSNextLevel b) -< StartGame `IN.pressBool` i

  returnA -< StartMenuOutputs $ StartMenuOutputsData{..}

  where [g1, g2, g3, g4, g5] = splitN 5 (randGen b)

v3ToV4 :: V3 a -> a -> V4 a 
v3ToV4 (V3 x y z) = V4 x y z

v4ToV3 :: V4 a -> V3 a 
v4ToV3 (V4 x y z _) = V3 x y z

reflected :: (RealFrac a) => (a, a) -> a -> a 
reflected (low, high) _ | low > high = error "reflected function given invalid boundaries (low > high)"
reflected (low, high) x = do
  let xMinusLow = x - low 
      highMinusLow = high - low 
      m = xMinusLow / highMinusLow
      floorM = floor m 
      q = fromIntegral floorM * highMinusLow
  if even floorM
  then x - q
  else q + high + low - x

---

introSF :: IntroConfigs -> Baton -> SF UserInputs GameOutputs
introSF IntroConfigs{..} b = proc userI -> do
  let i = IN.keyRemapToHM userI introInputMap
      updatedBaton = b { currentLevel = level, nextLevel = succ level}
      introColOut = introCol
      quitIntro = Quit `IN.pressBool` i
  leaveIntroEdge <- edge -< LeaveIntro `IN.pressBool` i
  let leaveIntroEvent = tag leaveIntroEdge (MSLeaveIntro updatedBaton)

  returnA -< IntroOutputs $ IntroOutputsData{..}

---

playingSF :: PlayingConfigs -> Baton -> SF UserInputs GameOutputs
playingSF PlayingConfigs{..} b = proc userI -> do
  let i = IN.keyRemapToHM userI playingInputMap
  
  timeTotal <- localTime -< ()
  timePrev <- iPre 0 -< timeTotal
  
  boxSize <- arrPrim round 
    <<< iterFrom (stepIntegralWithMin 2) (fromIntegral initialBoxSize) 
    <<< IN.quantifyInputPairSF (-0.05, -0.05, 0, 0.05, 0.05) 
    -< (IncreaseBoxSize !!! i, DecreaseBoxSize !!! i)
  -- TODO: add minBoxSize and maxBoxSize to Configs
  boxSPrev <- iPre initialBoxSize -< boxSize

  viewOffsets <- arrPrim (twoF round)
    <<< iterFrom (offsetAdjust (windowDim b)) initialOffsets 
    <<< identity *** arrPrim (twoF $ IN.quantifyInputPair (-2, -1, 0, 1, 2))
    -< (boxSize, ((MoveViewRight !!! i, MoveViewLeft !!! i), (MoveViewDown !!! i, MoveViewUp !!! i)))
  offsetsPrev <- iPre (twoF round $ initialOffsets) -< viewOffsets

  let extraBirths = filter (`inClosedTwoDim` userBounds) $ map convertPV2ToXY (zip3 (repeat boxSPrev) (repeat offsetsPrev) (IN.clicksPressedOrHeld userI ButtonLeft))
      extraDeaths = filter (`inClosedTwoDim` userBounds) $ map convertPV2ToXY (zip3 (repeat boxSPrev) (repeat offsetsPrev) (IN.clicksPressedOrHeld userI ButtonRight))

  enactCellsEvent <- edge -< EnactCells `IN.pressBool` i
  let enactCellsEventSFTagged = tag enactCellsEvent (sscan addCoOrdsToPendings (LH.emptyGrid, LH.emptyGrid))

  (pendingBirths, pendingDeaths) <- drSwitch (sscan addCoOrdsToPendings (LH.emptyGrid, LH.emptyGrid)) -< ((extraBirths, extraDeaths), enactCellsEventSFTagged)

  let dumpPendingsEvent = tag enactCellsEvent (pendingBirths, pendingDeaths)

  lifeDelayEvent <-  accumBy (sumWithMin minLifeDelay) initialLifeDelay
    <<< arrPrim (filterE (/= 0))
    <<< arrPrim Event
    <<< IN.quantifyInputPairSF (-4, -4, 0, 4, 4)
    -< (IncreaseLifeDelay !!! i, DecreaseLifeDelay !!! i)
  autoNextlifeEvent <- rSwitch (repeatedly initialLifeDelay ()) -< ((), fmap (`repeatedly` ()) lifeDelayEvent) 

  userNextlifeEvent <- edge -< NextLife `IN.pressBool` i
  let nextlifeEvent = lMerge autoNextlifeEvent userNextlifeEvent

  (aliveGrid, checkGrid) <- sscan (lifeUpdateBounded simBounds) initialLife -< (nextlifeEvent, dumpPendingsEvent)
  totalAlive <- arrPrim HM.size -< aliveGrid

  menuPressed <- edgeTag (MSPauseLevel b) -< Pause `IN.pressBool` i

  let windowDimPO = windowDim b
      userBoundsPO = userBounds
      simBoundsPO = simBounds
      timeJump = timeTotal - timePrev
      nextLifeBool = isEvent nextlifeEvent
      quitPlaying = Quit `IN.pressBool` i

  let outputsUnscored = PlayingOutputs $ PlayingOutputsData{..} 
  -- Partial record declaration. But this enforces the following rules:
  -- 1) scoreMeasure can't depend on the score or switchEvent
  -- 2) winTest and loseTest can depend on the score but not the switchEvent
  
  let score = applyScoreMeasure scoreMeasure outputsUnscored
      outputsScored = updateScore outputsUnscored score

  let winBool = applyPOTest winTest outputsScored
      loseBool = applyPOTest loseTest outputsScored
  wonLevel <- edgeTag (MSWinLevel b) -< winBool
  lostLevel <- edgeTag (MSLoseLevel b) -< loseBool
  let switchEvent = lMerge wonLevel (lMerge lostLevel menuPressed)

  returnA -< updateSwitchEvent outputsScored switchEvent

applyScoreMeasure :: ScoreMeasure -> GameOutputs -> Int 
applyScoreMeasure TotalAliveNow (PlayingOutputs pod) = totalAlive pod
applyScoreMeasure TimePassed    (PlayingOutputs pod) = round $ timeTotal pod
applyScoreMeasure _ _ = error "applyScoreMeasure: applied to wrong GameOutputs subtype"

applyPOTest :: PlayingOutputsTest -> GameOutputs-> Bool
applyPOTest (ScoreOver x) (PlayingOutputs pod) = score pod >= x 
applyPOTest (TimeOver x)  (PlayingOutputs pod) = timeTotal pod > x
applyPOTest _ _ = error "applyPOTest: applied to wrong GameOutputs subtype"

---

winScreenSF :: WinScreenConfigs -> Baton -> SF UserInputs GameOutputs
winScreenSF WinScreenConfigs{..} b = proc userI -> do
  let i = IN.keyRemapToHM userI winScreenInputMap
      winScreenColOut = winScreenCol
      quitWS = Quit `IN.pressBool` i
  nextLevelEvent <- edgeTag (MSNextLevel b) -< NextLevel `IN.pressBool` i
  returnA -< WinScreenOutputs $ WinScreenOutputsData{..}

---

loseScreenSF :: LoseScreenConfigs -> Baton -> SF UserInputs GameOutputs
loseScreenSF LoseScreenConfigs{..} b = proc userI -> do
  let i = IN.keyRemapToHM userI $ loseScreenInputMap
      loseScreenColOut = loseScreenCol
      quitLS = Quit `IN.pressBool` i
  retryLevelEvent <- edgeTag (MSRestartLevel b) -< RetryLevel `IN.pressBool` i
  
  returnA -< LoseScreenOutputs $ LoseScreenOutputsData{..}

---

endScreenSF :: EndScreenConfigs -> Baton -> SF UserInputs GameOutputs
endScreenSF EndScreenConfigs{..} b = proc userI -> do
  let i = IN.keyRemapToHM userI $ endScreenInputMap
      endScreenColOut = endScreenCol
      quitES = Quit `IN.pressBool` i
  
  returnA -< EndScreenOutputs $ EndScreenOutputsData{..}

---

(!!!) :: (Eq k, Hashable k) => k -> HM.HashMap k v -> Maybe v
(!!!) = HM.lookup 

loweringEdge :: Bool -> Bool -> Maybe ()
loweringEdge True False = Just ()
loweringEdge _    _     = Nothing

updateScore :: GameOutputs -> Int -> GameOutputs
updateScore (PlayingOutputs x) s = PlayingOutputs (x {score = s})
updateScore _                  _ = error "updateScore applied to wrong GameOutputs subtype"

updateSwitchEvent :: GameOutputs -> Event ModeSwitch -> GameOutputs
updateSwitchEvent (PlayingOutputs x) e = PlayingOutputs (x {switchEvent = e})
updateSwitchEvent _                  _ = error "updateSwitchEvent applied to wrong GameOutputs subtype"

lifeUpdateBounded :: XYBounds -> (Grid, Grid) -> (Event (), Event (Grid, Grid)) -> (Grid, Grid)
lifeUpdateBounded _         lifeCurrent (NoEvent, NoEvent)           = lifeCurrent
lifeUpdateBounded _         lifeCurrent (NoEvent, Event pendings)    = lifeCurrent `addExtraBirthsDeaths` pendings
lifeUpdateBounded simBounds lifeCurrent (Event _, dumpPendingsEvent) = lifeUpdateBounded simBounds lifeNew (NoEvent, dumpPendingsEvent) 
  where lifeNew = LH.simpleLifeBounded simBounds lifeCurrent

addCoOrdsToPendings :: (Grid, Grid) -> ([(Int, Int)], [(Int, Int)]) -> (Grid, Grid)
addCoOrdsToPendings (birthsGrid, deathsGrid) ([],   [])   = (birthsGrid, deathsGrid)
addCoOrdsToPendings (birthsGrid, deathsGrid) (b:bs, ds)   = addCoOrdsToPendings (LH.insert b birthsGrid, LH.delete b deathsGrid) (bs, ds)
addCoOrdsToPendings (birthsGrid, deathsGrid) ([],   d:ds) = addCoOrdsToPendings (LH.delete d birthsGrid, LH.insert d deathsGrid) ([], ds) 

addExtraBirthsDeaths :: (Grid, Grid) -> (Grid, Grid) -> (Grid, Grid)
addExtraBirthsDeaths (life_grid, check_grid) (birthsGrid, deathsGrid) = 
  ((life_grid `LH.unionGrid` birthsGrid) `LH.differenceGrid` deathsGrid, check_grid `LH.unionGrid` LH.addNeighbours (birthsGrid `LH.unionGrid` deathsGrid))

inClosedTwoDim :: Ord a => (a, a) -> (a, a, a, a) -> Bool
inClosedTwoDim (x,y) (xMin, xMax, yMin, yMax) = and [x >= xMin, x <= xMax, y >= yMin, y <= yMax]

sumWithMin :: (Num a, Ord a) => a -> a -> a -> a
sumWithMin mini x y = if s > mini then s else mini where s = x + y

-- Simple efficient calc, since we're dealing with step functions, x1 is ignored
-- Used by iterFrom
stepIntegral :: (Num a, Ord a, Fractional a) => a -> a -> DTime -> a -> a
stepIntegral x0 _ dt y0 = y0 + (x0 * realToFrac dt)

stepIntegralWithMin :: (Num a, Ord a, Fractional a) => a -> a -> a -> DTime -> a -> a
stepIntegralWithMin mini x0 x1 dt y0 = if y1 > mini then y1 else mini 
  where y1 = stepIntegral x0 x1 dt y0

stepIntegralWithMinMax :: (Num a, Ord a, Fractional a) => (a, a) -> a -> a -> DTime -> a -> a
stepIntegralWithMinMax (mini, maxi) x0 x1 dt y0
  | y1 < mini = mini
  | y1 > maxi = maxi
  | otherwise = y1
  where y1 = stepIntegral x0 x1 dt y0

convertPV2ToXY :: (CInt, (CInt, CInt), Point V2 Int32) -> (Int, Int)
convertPV2ToXY (box_size, (offsetX, offsetY), P (V2 x y)) = 
  (fromIntegral $ (x' + offsetX) `div` box_size, fromIntegral $ (y' + offsetY) `div` box_size) where
    (x', y') = (fromIntegral x, fromIntegral y)

offsetAdjust :: (CInt, CInt) -> (CInt, (Double, Double)) -> (CInt, (Double, Double)) -> DTime -> (Double, Double) -> (Double, Double)
offsetAdjust (windowW, windowH) (boxSize0, (adjustX0, adjustY0)) (boxSize1, (_,_)) dt (offsetX0, offsetY0) = 
  if boxSize0 == boxSize1
  then (adjustX0 * dt + offsetX0, adjustY0 * dt + offsetY0)
  else (adjustX0 * dt + boxSizeR * offsetX0 + fromIntegral windowW * (boxSizeR - 1) / 2, adjustY0 * dt + boxSizeR * offsetY0 + fromIntegral windowH * (boxSizeR - 1) / 2) where 
    boxSizeR = fromIntegral boxSize1 / fromIntegral boxSize0

twoF :: (a -> b) -> (a, a) -> (b, b)
twoF f (x, y) = (f x, f y)

threeF :: (a -> b) -> (a, a, a) -> (b, b, b)
threeF f (x, y, z) = (f x, f y, f z)
