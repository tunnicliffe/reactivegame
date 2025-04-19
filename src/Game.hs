{-# LANGUAGE NoImplicitPrelude #-}

module Game (gameSF) where

import ClassyPrelude
import FRP.Yampa
import Types
import Types.Configs

import qualified Data.HashMap.Strict as HM
import qualified GameLogic as GL


gameSF :: GameConfigs -> Baton -> SF UserInputs GameOutputs
gameSF gc _ | null (levelConfigsMap gc) = error "No levels to play!"
gameSF gc baton = kSwitch (GL.startMenuSF (startMenuConfigs gc) baton) switchModeDetectSF (masterRouter gc)
-- ^ Start on the startMenuSF, listen for ModeSwitch with switchModeDetectSF, change modes via masterRouter

---

switchModeDetectSF :: SF (UserInputs, GameOutputs) (Event ModeSwitch)
switchModeDetectSF = arrPrim switchModeDetect

switchModeDetect :: (UserInputs, GameOutputs) -> Event ModeSwitch
switchModeDetect (_, PlayingOutputs o)    = switchEvent o
switchModeDetect (_, PauseMenuOutputs o)  = unpauseEvent o -- plus lMerge a load event?
switchModeDetect (_, StartMenuOutputs o)  = startGameEvent o
switchModeDetect (_, IntroOutputs o)      = leaveIntroEvent o
switchModeDetect (_, WinScreenOutputs o)  = nextLevelEvent o
switchModeDetect (_, LoseScreenOutputs o) = retryLevelEvent o
switchModeDetect (_, EndScreenOutputs o)  = NoEvent
-- ^ TODO: We could directly listen for quitting or pausing inputs.
-- Advantage being that we wouldn't need a different quitIntro, quitPlaying, etc.

masterRouter :: GameConfigs -> SF UserInputs GameOutputs -> ModeSwitch -> SF UserInputs GameOutputs
masterRouter gc currentSF (MSPauseLevel b) = kSwitch (GL.pauseMenuSF (pauseMenuConfigs gc) b) switchModeDetectSF (pausedRouter gc currentSF)
masterRouter gc _ (MSNextLevel b)    = kSwitch (GL.introSF (introConfigs (nextLevConf gc b)) b)              switchModeDetectSF (masterRouter gc)
masterRouter gc _ (MSLeaveIntro b)   = kSwitch (GL.playingSF (playingConfigs (currentLevConf gc b)) b)       switchModeDetectSF (masterRouter gc)
masterRouter gc _ (MSWinLevel b)     = kSwitch (GL.winScreenSF (winScreenConfigs (currentLevConf gc b)) b)   switchModeDetectSF (masterRouter gc)
masterRouter gc _ (MSLoseLevel b)    = kSwitch (GL.loseScreenSF (loseScreenConfigs (currentLevConf gc b)) b) switchModeDetectSF (masterRouter gc)
masterRouter gc _ (MSRestartLevel b) = kSwitch (GL.introSF (introConfigs (currentLevConf gc b)) b)           switchModeDetectSF (masterRouter gc)
--masterRouter _  _ (MSLoadLevel storedSF) = kSwitch storedSF switchModeDetectSF $ masterRouter gc

-- | Need additional routing function to store the game while paused
pausedRouter :: GameConfigs -> SF UserInputs GameOutputs -> SF UserInputs GameOutputs -> ModeSwitch -> SF UserInputs GameOutputs
pausedRouter gc storedSF _ MSUnpauseLevel  = kSwitch storedSF                                 switchModeDetectSF (masterRouter gc)
pausedRouter gc _        _ (MSLoadLevel b) = kSwitch (GL.pauseMenuSF (pauseMenuConfigs gc) b) switchModeDetectSF (pausedRouter gc loadedSF)
  where loadedSF = kSwitch (GL.playingSF (playingConfigs (currentLevConf gc b)) b) switchModeDetectSF (masterRouter gc)

---

currentLevConf :: GameConfigs -> Baton -> LevelConfigs
currentLevConf gc b = HM.lookupDefault (error ("LevelType not in levelConfigsMap: " <> show x)) x l
  where
  x = currentLevel b 
  l = levelConfigsMap gc

nextLevConf :: GameConfigs -> Baton -> LevelConfigs
nextLevConf gc b = HM.lookupDefault (error ("LevelType not in levelConfigsMap: " <> show y)) y l
  where
  y = nextLevel b 
  l = levelConfigsMap gc
