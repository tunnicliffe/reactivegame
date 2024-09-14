{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Configs
  ( GameConfigs(..)
  , PauseMenuConfigs(..)
  , StartMenuConfigs(..)
  , LevelConfigs(..)
  , IntroConfigs(..)
  , PlayingConfigs(..)
  , ScoreMeasureType(..)
  , PlayingOutputsTest(..)
  , WinScreenConfigs(..)
  , LoseScreenConfigs(..)
  , EndScreenConfigs(..)
  ) where

import ClassyPrelude

import Data.Aeson (FromJSON, FromJSONKey, ToJSON)
import Foreign.C.Types (CInt)
import FRP.Yampa (Time)
import SDL (V4)
import Types (Colour, XYBounds, Grid, LevelType, InputKey, InputAction)


-- Immutable variables that can only be accessed by the SF UserInputs GameOutputs in GameLogic.hs
data GameConfigs = GameConfigs 
  { pauseMenuConfigs  :: PauseMenuConfigs
  , startMenuConfigs  :: StartMenuConfigs
  , levelConfigsMap   :: HashMap LevelType LevelConfigs
  , endScreenConfigs  :: EndScreenConfigs
  } deriving (Generic)
instance FromJSON GameConfigs

data PauseMenuConfigs = PauseMenuConfigs 
  { pauseMenuInputMap :: HashMap InputKey InputAction
  , pauseMenuCol      :: Colour
  } deriving (Generic)
instance FromJSON PauseMenuConfigs

data StartMenuConfigs = StartMenuConfigs 
  { startMenuInputMap     :: HashMap InputKey InputAction
  , startMenuCol          :: V4 Double
  , ballCol               :: Colour
  , startMenuRGSeed       :: Int
  } deriving (Generic)
instance FromJSON StartMenuConfigs

data LevelConfigs = LevelConfigs 
  { introConfigs      :: IntroConfigs
  , playingConfigs    :: PlayingConfigs
  , winScreenConfigs  :: WinScreenConfigs
  , loseScreenConfigs :: LoseScreenConfigs
  } deriving (Generic)
instance FromJSON LevelConfigs

data IntroConfigs = IntroConfigs 
  { level             :: LevelType
  , introInputMap     :: HashMap InputKey InputAction
  , introCol          :: Colour
  } deriving (Generic)
instance FromJSON IntroConfigs

data PlayingConfigs = PlayingConfigs 
  { playingInputMap     :: HashMap InputKey InputAction
  , simBounds           :: XYBounds
  , userBounds          :: XYBounds
  , initialLife         :: (Grid, Grid)
  , initialLifeDelay    :: Time
  , minLifeDelay        :: Time
  , initialOffsets      :: (Double, Double) 
  , initialBoxSize      :: CInt
  , scoreMeasure        :: ScoreMeasureType
  , winTest             :: PlayingOutputsTest 
  , loseTest            :: PlayingOutputsTest
  } deriving (Generic)
instance FromJSON PlayingConfigs
  -- scoreMeasure MUST NOT depend on score or switchEvent
  -- win/loseTest MUST NOT depend on switchEvent (but can depend on score)
  -- This is because they're implemented using omitted fields in the record syntax

data ScoreMeasureType
  = TotalAliveNow
  | TimePassed
  deriving (Generic)
instance FromJSON ScoreMeasureType

data PlayingOutputsTest
  = ScoreOver Int
  | TimeOver Time
  deriving (Generic)
instance FromJSON PlayingOutputsTest

data WinScreenConfigs = WinScreenConfigs
  { winScreenInputMap     :: HashMap InputKey InputAction
  , winScreenCol          :: Colour
  } deriving (Generic)
instance FromJSON WinScreenConfigs

data LoseScreenConfigs = LoseScreenConfigs
  { loseScreenInputMap      :: HashMap InputKey InputAction
  , loseScreenCol           :: Colour
  } deriving (Generic)
instance FromJSON LoseScreenConfigs

data EndScreenConfigs = EndScreenConfigs
  { endScreenInputMap     :: HashMap InputKey InputAction
  , endScreenCol          :: Colour
  } deriving (Generic)
instance FromJSON EndScreenConfigs
