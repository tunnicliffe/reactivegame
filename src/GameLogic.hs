{-# LANGUAGE Arrows #-}

module GameLogic (gameSF) where

import FRP.Yampa
import SDL              (V2(..), V3(..), V4(..), Point(..), MouseButton(..))
import Foreign.C.Types  (CInt)
import Data.Int         (Int32)
import Data.Hashable    (Hashable)
import Data.Maybe       (fromJust)

import Types
import Stochastic       (brownianMotion2D, brownianMotion3D)

import qualified Data.HashMap.Strict  as HM
import qualified Input                as IN
import qualified LifeHash             as LH

---

gameSF :: GameConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
gameSF gc batons | null (levelConfigsMap gc) = error "No levels to play!"
gameSF gc batons = kSwitch (startMenuSF (startMenuConfigs gc) batons) switchModeDetectSF (masterRouter gc) 

---

switchModeDetectSF :: SF (UserInputs, GameOutputs) (Event ModeSwitch)
switchModeDetectSF = arrPrim switchModeDetect
switchModeDetect :: (UserInputs, GameOutputs) -> Event ModeSwitch
switchModeDetect (_, PlayingOutputs o)    = switchEvent o
switchModeDetect (_, PauseMenuOutputs o)  = lMerge (unpauseEvent o) (loadEvent o)
switchModeDetect (_, StartMenuOutputs o)  = startGameEvent o
switchModeDetect (_, IntroOutputs o)      = leaveIntroEvent o
switchModeDetect (_, WinScreenOutputs o)  = nextLevelEvent o
switchModeDetect (_, LoseScreenOutputs o) = retryLevelEvent o
switchModeDetect (_, EndScreenOutputs o)  = NoEvent
--TODO: We could directly listen for quitting or pausing inputs.
-- Advantage being that we wouldn't need a different quitIntro, quitPlaying, etc.

masterRouter :: GameConfigs -> SF UserInputs GameOutputs -> ModeSwitch -> SF UserInputs GameOutputs
masterRouter gc currentSF (PauseLevelMS batons)   = kSwitch (pauseMenuSF (pauseMenuConfigs gc) batons)                           switchModeDetectSF (pausedRouter gc currentSF)
masterRouter gc _         (NextLevelMS batons)    = kSwitch (introSF (introConfigs (nextLevConf gc batons)) batons)              switchModeDetectSF (masterRouter gc)
masterRouter gc _         (LeaveIntroMS batons)   = kSwitch (playingSF (playingConfigs (currentLevConf gc batons)) batons)       switchModeDetectSF (masterRouter gc)
masterRouter gc _         (WinLevelMS batons)     = kSwitch (winScreenSF (winScreenConfigs (currentLevConf gc batons)) batons)   switchModeDetectSF (masterRouter gc)
masterRouter gc _         (LoseLevelMS batons)    = kSwitch (loseScreenSF (loseScreenConfigs (currentLevConf gc batons)) batons) switchModeDetectSF (masterRouter gc)
masterRouter gc _         (RestartLevelMS batons) = kSwitch (introSF (introConfigs (currentLevConf gc batons)) batons)           switchModeDetectSF (masterRouter gc)
--masterRouter _  _ (LoadLevelMS storedSF) = kSwitch storedSF switchModeDetectSF $ masterRouter gc

--Need additional routing function to store the game while paused:
pausedRouter :: GameConfigs -> SF UserInputs GameOutputs -> SF UserInputs GameOutputs -> ModeSwitch -> SF UserInputs GameOutputs
pausedRouter gc storedSF _ UnpauseLevelMS       = kSwitch storedSF                                   switchModeDetectSF (masterRouter gc)
pausedRouter gc _        _ (LoadLevelMS batons) = kSwitch (pauseMenuSF (pauseMenuConfigs gc) batons) switchModeDetectSF (pausedRouter gc loadedSF) where
  loadedSF = kSwitch (playingSF (playingConfigs (currentLevConf gc batons)) batons) switchModeDetectSF (masterRouter gc)

---

currentLevConf :: GameConfigs -> [Maybe Baton] -> LevelConfigs
currentLevConf gc bs = HM.lookupDefault (error ("LevelID not in levelConfigsMap: " ++ show x)) x l where
  x = currentLevel $ headB bs 
  l = levelConfigsMap gc

nextLevConf :: GameConfigs -> [Maybe Baton] -> LevelConfigs
nextLevConf gc bs = HM.lookupDefault (error ("LevelID not in levelConfigsMap: " ++ show y)) y l where
  y = nextLevel $ headB bs 
  l = levelConfigsMap gc

headB :: [Maybe Baton] -> Baton 
headB = fromJust . head

---

pauseMenuSF :: PauseMenuConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
pauseMenuSF pmConf bs = proc userI -> do
  let i = IN.keyRemapToHM userI $ pauseMenuInputMap pmConf
  let baton = headB bs

  unpauseE <- dropEvents 1 <<< edgeTag UnpauseLevelMS -< Unpause `IN.releaseBool` i

  save1E <- edgeTag 1 -< Save1 `IN.pressBool` i 
  save2E <- edgeTag 2 -< Save2 `IN.pressBool` i
  save3E <- edgeTag 3 -< Save3 `IN.pressBool` i
  save4E <- edgeTag 4 -< Save4 `IN.pressBool` i
  save5E <- edgeTag 5 -< Save5 `IN.pressBool` i
  save6E <- edgeTag 6 -< Save6 `IN.pressBool` i
  save7E <- edgeTag 7 -< Save7 `IN.pressBool` i
  save8E <- edgeTag 8 -< Save8 `IN.pressBool` i
  save9E <- edgeTag 9 -< Save9 `IN.pressBool` i
  let saveE = foldl1 lMerge [save1E, save2E, save3E, save4E, save5E, save6E, save7E, save8E, save9E] `attach` baton

  let bsUpdated = saveE `savesUpdate` bs

  load1E <- edgeTag 1 -< Load1 `IN.pressBool` i 
  load2E <- edgeTag 2 -< Load2 `IN.pressBool` i 
  load3E <- edgeTag 3 -< Load3 `IN.pressBool` i 
  load4E <- edgeTag 4 -< Load4 `IN.pressBool` i 
  load5E <- edgeTag 5 -< Load5 `IN.pressBool` i 
  load6E <- edgeTag 6 -< Load6 `IN.pressBool` i 
  load7E <- edgeTag 7 -< Load7 `IN.pressBool` i 
  load8E <- edgeTag 8 -< Load8 `IN.pressBool` i 
  load9E <- edgeTag 9 -< Load9 `IN.pressBool` i 
  let loadE = foldl1 lMerge [load1E, load2E, load3E, load4E, load5E, load6E, load7E, load8E, load9E] -- *** Fix loading here
  let loadEB = loadBaton loadE bsUpdated

  returnA -< PauseMenuOutputs $ PauseMenuOutputsData { pauseMenuColOut = pauseMenuCol pmConf
                                                     , unpauseEvent = unpauseE
                                                     , saveEvent = saveE
                                                     , loadEvent = loadEB
                                                     , quitPM = Quit `IN.pressBool` i
                                                     }

savesUpdate :: Event (Int, Baton) -> [Maybe Baton] -> [Maybe Baton]
savesUpdate NoEvent = id 
savesUpdate (Event (slot, b)) = setAt slot (Just b)

setAt :: Int -> a -> [a] -> [a]
setAt n _ _      | n < 0 = error "SetAt: Negative index "
setAt _ _ []     = error "SetAt: Invalid index for given list"
setAt 0 x (_:ys) = x : ys
setAt n x (y:ys) = y : setAt (n-1) x ys

loadBaton :: Event Int -> [Maybe Baton] -> Event ModeSwitch
loadBaton NoEvent _ = NoEvent
loadBaton (Event n) bs = case bs !! n of 
  Nothing -> NoEvent
  x       -> Event . LoadLevelMS $ x : tail bs

---

startMenuSF :: StartMenuConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
startMenuSF smConf bs = proc userI -> do
  let i = IN.keyRemapToHM userI $ startMenuInputMap smConf
  bm3D <- brownianMotion3D (v4ToV3 $ startMenuCol smConf) 0.05 (head (randGen (headB bs)), randGen (headB bs) !! 1, randGen (headB bs) !! 2) -< ()
  bm2D <- brownianMotion2D (V2 100 100) 2.0 (randGen (headB bs) !! 3, randGen (headB bs) !! 4) -< ()
  bgCol <- arrPrim (fmap (fromInteger . round)) <<< arrPrim (fmap (reflected (0, 255))) -< v3ToV4 bm3D 255
  bP <- arrPrim (fmap round) <<< arrPrim (fmap (reflected (0,400))) -< bm2D 

  startGameEvent <- edgeTag (NextLevelMS bs) -< StartGame `IN.pressBool` i

  returnA -< StartMenuOutputs $ StartMenuOutputsData {startMenuColOut = bgCol, ballColOut = ballCol smConf, ballPos = bP, startGameEvent = startGameEvent, quitSM = Quit `IN.pressBool` i}

v3ToV4 :: V3 a -> a -> V4 a 
v3ToV4 (V3 x y z) = V4 x y z

v4ToV3 :: V4 a -> V3 a 
v4ToV3 (V4 x y z _) = V3 x y z

reflected :: (RealFrac a) => (a, a) -> a -> a 
reflected (low, high) x | low > high = error "reflected function given invalid boundaries (low > high)"
reflected (low, high) x =
  let 
    xMinusLow = x - low 
    highMinusLow = high - low 
    m = xMinusLow / highMinusLow
    floorM = floor m 
    q = fromIntegral floorM * highMinusLow
  in
    if even floorM
      then x - q
      else q + high + low - x

---

introSF :: IntroConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
introSF iConf bs = proc userI -> do
  let i = IN.keyRemapToHM userI $ introInputMap iConf
  let baton = headB bs

  let updatedBaton = baton { currentLevel = level iConf
                           , nextLevel    = if level iConf == Level1 then Level2 else StartScreen} -- NEEDS FIX
  let updatedBs = Just updatedBaton : tail bs

  leaveIntroEvent <- edge -< LeaveIntro `IN.pressBool` i

  returnA -< IntroOutputs $ IntroOutputsData { introColOut = introCol iConf
                                             , leaveIntroEvent = tag leaveIntroEvent (LeaveIntroMS updatedBs)
                                             , quitIntro = Quit `IN.pressBool` i
                                             }

---

playingSF :: PlayingConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
playingSF pConf bs = proc userI -> do
  let i = IN.keyRemapToHM userI $ playingInputMap pConf
  let baton = headB bs

  timeNow <- localTime -< ()
  timePrev <- iPre 0 -< timeNow

  boxS  <-  arrPrim round 
        <<< iterFrom (stepIntegralWithMin 2) (fromIntegral $ initialBoxSize pConf) 
        <<< IN.quantifyInputPairSF (-0.05, -0.05, 0, 0.05, 0.05) 
        -<  (IncreaseBoxSize !!! i, DecreaseBoxSize !!! i)
  --minBoxSize and maxBoxSize due to be added to Configs
  boxSPrev <- iPre (initialBoxSize pConf) -< boxS

  offsets <-  arrPrim (twoF round)
          <<< iterFrom (offsetAdjust (windowDim (headB bs))) (initialOffsets pConf) 
          <<< identity *** arrPrim (twoF $ IN.quantifyInputPair (-2, -1, 0, 1, 2))
          -<  (boxS, ((MoveViewRight !!! i, MoveViewLeft !!! i), (MoveViewDown !!! i, MoveViewUp !!! i)))
  offsetsPrev <- iPre (twoF round $ initialOffsets pConf) -< offsets

  let extraBirths = filter (`inClosedTwoDim` userBounds pConf) $ map convertPV2ToXY (zip3 (repeat boxSPrev) (repeat offsetsPrev) (IN.clicksPressedOrHeld userI ButtonLeft))
  let extraDeaths = filter (`inClosedTwoDim` userBounds pConf) $ map convertPV2ToXY (zip3 (repeat boxSPrev) (repeat offsetsPrev) (IN.clicksPressedOrHeld userI ButtonRight))

  enactCellsEvent <- edge -< EnactCells `IN.pressBool` i
  let enactCellsEventSFTagged = tag enactCellsEvent (sscan addCoOrdsToPendings (LH.emptyGrid, LH.emptyGrid))

  (pendingB, pendingD) <- drSwitch (sscan addCoOrdsToPendings (LH.emptyGrid, LH.emptyGrid)) -< ((extraBirths, extraDeaths), enactCellsEventSFTagged)

  let dumpPendingsEvent = tag enactCellsEvent (pendingB, pendingD)

  lifeDelayEvent  <-  accumBy (sumWithMin (minLifeDelay pConf)) (initialLifeDelay pConf) 
                  <<< arrPrim (filterE (/= 0)) 
                  <<< arrPrim Event 
                  <<< IN.quantifyInputPairSF (-4, -4, 0, 4, 4) 
                  -<  (IncreaseLifeDelay !!! i, DecreaseLifeDelay !!! i)
  autoNextlifeEvent <- rSwitch (repeatedly (initialLifeDelay pConf) ()) -< ((), fmap (`repeatedly` ()) lifeDelayEvent) 

  userNextlifeEvent <- edge -< NextLife `IN.pressBool` i

  let nextlifeEvent = lMerge autoNextlifeEvent userNextlifeEvent

  (aliveG, checkG) <- sscan (lifeUpdateBounded (simBounds pConf)) (initialLife pConf) -< (nextlifeEvent, dumpPendingsEvent)

  totalA <- arrPrim HM.size -< aliveG -- Inefficient

  menuPressed <- edgeTag (PauseLevelMS bs) -< Pause `IN.pressBool` i

  let outputsUnscored = PlayingOutputs $ PlayingOutputsData { windowDimPO   = windowDim baton 
                                                            , userBoundsPO  = userBounds pConf
                                                            , simBoundsPO   = simBounds pConf
                                                            , timeTotal     = timeNow
                                                            , timeJump      = timeNow - timePrev
                                                            , aliveGrid     = aliveG
                                                            , checkGrid     = checkG
                                                            , totalAlive    = totalA
                                                            , boxSize       = boxS
                                                            , viewOffsets   = offsets
                                                            , pendingBirths = pendingB
                                                            , pendingDeaths = pendingD
                                                            , nextLifeBool  = isEvent nextlifeEvent
                                                            , quitPlaying   = Quit `IN.pressBool` i
                                                            } 
  -- Partial record declaration. But this enforces the following rules:
  -- 1) scoreMeasure can't depend on the score or switchEvent
  -- 2) winTest and loseTest can depend on the score but not the switchEvent
  
  let score = applyScoreMeasure (scoreMeasure pConf) outputsUnscored
  let outputsScored = updateScore outputsUnscored score

  let winBool = applyPOTest (winTest pConf) outputsScored
  let loseBool = applyPOTest (loseTest pConf) outputsScored
  wonLevel <- edgeTag (WinLevelMS bs) -< winBool
  lostLevel <- edgeTag (LoseLevelMS bs) -< loseBool
  let switchEvent = lMerge wonLevel (lMerge lostLevel menuPressed)
  
  let outputsFinal = updateSwitchEvent outputsScored switchEvent

  returnA -< outputsFinal

applyScoreMeasure :: ScoreMeasure -> GameOutputs -> Int 
applyScoreMeasure TotalAliveNow (PlayingOutputs pod) = totalAlive pod
applyScoreMeasure TimePassed    (PlayingOutputs pod) = round $ timeTotal pod
applyScoreMeasure _ _ = error "applyScoreMeasure: applied to wrong GameOutputs subtype"

applyPOTest :: PlayingOutputsTest -> GameOutputs-> Bool
applyPOTest (ScoreOver x) (PlayingOutputs pod) = score pod >= x 
applyPOTest (TimeOver x)  (PlayingOutputs pod) = timeTotal pod > x
applyPOTest _ _ = error "applyPOTest: applied to wrong GameOutputs subtype"

---

winScreenSF :: WinScreenConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
winScreenSF wsConf bs = proc userI -> do
  let i = IN.keyRemapToHM userI $ winScreenInputMap wsConf
  nlEvent <- edgeTag (NextLevelMS bs) -< NextLevel `IN.pressBool` i
  returnA -< WinScreenOutputs $ WinScreenOutputsData {winScreenColOut = winScreenCol wsConf, nextLevelEvent = nlEvent, quitWS = Quit `IN.pressBool` i}

---

loseScreenSF :: LoseScreenConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
loseScreenSF lsConf bs = proc userI -> do
  let i = IN.keyRemapToHM userI $ loseScreenInputMap lsConf
  rlEvent <- edgeTag (RestartLevelMS bs) -< RetryLevel `IN.pressBool` i
  returnA -< LoseScreenOutputs $ LoseScreenOutputsData {loseScreenColOut = loseScreenCol lsConf, retryLevelEvent = rlEvent, quitLS = Quit `IN.pressBool` i}

---

endScreenSF :: EndScreenConfigs -> [Maybe Baton] -> SF UserInputs GameOutputs
endScreenSF esConf bs = proc userI -> do
  let i = IN.keyRemapToHM userI $ endScreenInputMap esConf
  returnA -< EndScreenOutputs $ EndScreenOutputsData {endScreenColOut = endScreenCol esConf, quitES = Quit `IN.pressBool` i}

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

lifeUpdateBounded :: XYBounds -> (LH.Grid, LH.Grid) -> (Event (), Event (LH.Grid, LH.Grid)) -> (LH.Grid, LH.Grid)
lifeUpdateBounded _         lifeCurrent (NoEvent, NoEvent)           = lifeCurrent
lifeUpdateBounded _         lifeCurrent (NoEvent, Event pendings)    = lifeCurrent `addExtraBirthsDeaths` pendings
lifeUpdateBounded simBounds lifeCurrent (Event _, dumpPendingsEvent) = lifeUpdateBounded simBounds lifeNew (NoEvent, dumpPendingsEvent) 
  where lifeNew = LH.simpleLifeBounded simBounds lifeCurrent

addCoOrdsToPendings :: (LH.Grid, LH.Grid) -> ([(Int, Int)], [(Int, Int)]) -> (LH.Grid, LH.Grid)
addCoOrdsToPendings (birthsGrid, deathsGrid) ([],   [])   = (birthsGrid, deathsGrid)
addCoOrdsToPendings (birthsGrid, deathsGrid) (b:bs, ds)   = addCoOrdsToPendings (LH.insert b birthsGrid, LH.delete b deathsGrid) (bs, ds)
addCoOrdsToPendings (birthsGrid, deathsGrid) ([],   d:ds) = addCoOrdsToPendings (LH.delete d birthsGrid, LH.insert d deathsGrid) ([], ds) 

addExtraBirthsDeaths :: (LH.Grid, LH.Grid) -> (LH.Grid, LH.Grid) -> (LH.Grid, LH.Grid)
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
