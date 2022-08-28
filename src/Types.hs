{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}

module Types 
  ( XYBounds
  , Colour

  , GameConfigs (..)
  , PauseMenuConfigs (..)
  , StartMenuConfigs (..)
  , LevelConfigs (..)
  , IntroConfigs (..)
  , PlayingConfigs (..)
  , LevelID (..)
  , ScoreMeasure (..)
  , PlayingOutputsTest (..)
  , WinScreenConfigs (..)
  , LoseScreenConfigs (..)
  , EndScreenConfigs (..)

  , DisplayConfigs (..)
  , DisplayResources (..)

  , UserInputs (..)
  , nullUserInputs
  , InputKey (..)
  , keycodeToIK
  , InputState (..)
  , InputAction (..)

  , ModeSwitch (..)
  , Baton (..)

  , GameOutputs (..)
  , PauseMenuOutputsData (..)
  , StartMenuOutputsData (..)
  , IntroOutputsData (..)
  , PlayingOutputsData (..)
  , WinScreenOutputsData (..)
  , LoseScreenOutputsData (..)
  , EndScreenOutputsData (..)
  ) where

import InputKey (InputKey (..), keycodeToIK)
import LifeHash (Grid)

import FRP.Yampa            (Event(..), Time, DTime, SF)
import SDL                  (V2(..), V3(..), V4(..), Point(..), Texture, Renderer)
import SDL.Framerate        (Framerate)
import SDL.Mixer            (Chunk)
import Foreign.C.Types      (CInt)
import Data.Int             (Int32)
import Data.Word            (Word8)
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap, empty)
import Data.Hashable        (Hashable)
import Data.VectorSpace     (VectorSpace (..))
import Data.Aeson           (FromJSON (..), FromJSONKey, ToJSON(..))
import GHC.Generics         (Generic)
import System.Random        (StdGen)

---
--Useful instances

instance RealFloat a => VectorSpace (V2 a) a where
  zeroVector = V2 0 0
  a *^ (V2 x y) = V2 (a * x) (a * y)
  (V2 x y) ^/ a = V2 (x / a) (y / a)
  negateVector (V2 x y) = (V2 (-x) (-y))
  (V2 x1 y1) ^+^ (V2 x2 y2) = V2 (x1+x2) (y1+y2)
  (V2 x1 y1) ^-^ (V2 x2 y2) = V2 (x1-x2) (y1-y2)
  (V2 x1 y1) `dot` (V2 x2 y2) = x1 * x2 + y1 * y2

instance RealFloat a => VectorSpace (V3 a) a where
  zeroVector = V3 0 0 0
  a *^ (V3 x y z) = V3 (a * x) (a * y) (a * z)
  (V3 x y z) ^/ a = V3 (x / a) (y / a) (z / a)
  negateVector (V3 x y z) = (V3 (-x) (-y) (-z))
  (V3 x1 y1 z1) ^+^ (V3 x2 y2 z2) = V3 (x1+x2) (y1+y2) (z1+z2)
  (V3 x1 y1 z1) ^-^ (V3 x2 y2 z2) = V3 (x1-x2) (y1-y2) (z1-z2)
  (V3 x1 y1 z1) `dot` (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Hashable InputKey
instance FromJSON InputKey
instance FromJSONKey InputKey
instance (FromJSON a) => FromJSON (V2 a)
instance (FromJSON a) => FromJSON (V3 a)
instance (FromJSON a) => FromJSON (V4 a)

instance FromJSON CInt where
  parseJSON v = toEnum <$> parseJSON v
instance ToJSON CInt where
  toJSON = toJSON . show
instance FromJSON StdGen where
  parseJSON v = read <$> parseJSON v
instance ToJSON StdGen where
  toJSON = toJSON . show 

---

type XYBounds = (Int, Int, Int, Int) --x_min,x_max,y_min,y_max

type Colour = V4 Word8

---

-- Immutable variables that can only be accessed by the GameLogic SFs
data GameConfigs = GameConfigs 
  { pauseMenuConfigs  :: PauseMenuConfigs
  , startMenuConfigs  :: StartMenuConfigs
  , levelConfigsMap   :: HashMap LevelID LevelConfigs
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
  { level             :: LevelID
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
  , scoreMeasure        :: ScoreMeasure
  , winTest             :: PlayingOutputsTest 
  , loseTest            :: PlayingOutputsTest
  } deriving (Generic)
instance FromJSON PlayingConfigs
  -- scoreMeasure MUST NOT depend on score or switchEvent
  -- win/loseTest MUST NOT depend on switchEvent (but can depend on score)
  -- This is because they're implemented using omitted fields in the record syntax

data LevelID = 
  StartScreen |
  Level1 |
  Level2
    deriving (Generic, Eq, Show, Read)
instance Hashable LevelID
instance FromJSON LevelID
instance FromJSONKey LevelID
instance ToJSON LevelID

data ScoreMeasure = 
  TotalAliveNow | 
  TimePassed
    deriving (Generic)
instance FromJSON ScoreMeasure

data PlayingOutputsTest = 
  ScoreOver Int |
  TimeOver Time
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

---

-- Immuatable variables that can only be accessed by the displayFunction

-- DisplayConfigs are parsed at runtime from a config file 
data DisplayConfigs = DisplayConfigs
  { initialWindowDim  :: (CInt, CInt)
  , windowName        :: Text  
  , bgCol             :: Colour
  , lineCol           :: Colour
  , aliveCol          :: Colour
  , checkCol          :: Colour
  , userAreaCol       :: Colour
  , pendingBirthCol   :: Colour
  , pendingDeathCol   :: Colour
  , framerateLimit    :: Framerate
  , textFontSize      :: Int 
  , textCol           :: Colour
  , counterFontSize   :: Int
  , counterCol        :: Colour
  , scoreOffset       :: V2 CInt
  , timeOffset        :: V2 CInt
  , fpsOffset         :: V2 CInt
  , fontPath          :: FilePath
  } deriving (Generic)
instance FromJSON DisplayConfigs

-- DisplayResources are loaded at runtime using some values from the DisplayConfigs
data DisplayResources = DisplayResources 
  { renderer          :: Renderer 
  , scoreTexture      :: Texture
  , timeTexture       :: Texture
  , fpsTexture        :: Texture
  , digitTextures     :: [Texture]
  , charTextureMap    :: HashMap Char Texture
  , audioChunk        :: Chunk
  }

---

data UserInputs = UserInputs 
  { keyboardInputs    :: [(InputKey, InputState)]
  , mouseLeftInputs   :: [(Point V2 Int32, InputState)]
  , mouseRightInputs  :: [(Point V2 Int32, InputState)]
  , quitBool          :: Bool
  }

nullUserInputs = UserInputs 
  { keyboardInputs    = []
  , mouseLeftInputs   = []
  , mouseRightInputs  = []
  , quitBool          = False
  }

data InputState = Press | Hold | Release 
  deriving (Eq)

data InputAction =
  Pause |
  Unpause |
  Quit |
  StartGame |
  LeaveIntro |
  IncreaseBoxSize | 
  DecreaseBoxSize |
  MoveViewRight |
  MoveViewLeft |
  MoveViewDown |
  MoveViewUp |
  EnactCells |
  IncreaseLifeDelay |
  DecreaseLifeDelay |
  NextLife |
  NextLevel |
  RetryLevel |
  Save1 |
  Load1 |
  Save2 |
  Load2 |
  Save3 |
  Load3 |
  Save4 |
  Load4 |
  Save5 |
  Load5 |
  Save6 |
  Load6 |
  Save7 |
  Load7 |
  Save8 |
  Load8 |
  Save9 |
  Load9 
    deriving (Generic, Eq)
instance FromJSON InputAction
instance Hashable InputAction

---

-- Switching mode means chaning the (SF UserInputs GameOutputs) currently in use
-- This is used to traverse between the modes of the game whilst retaining the Baton
data ModeSwitch = 
  PauseLevelMS [Maybe Baton] |
  UnpauseLevelMS | 
  LoadLevelMS [Maybe Baton] |
  LeaveIntroMS [Maybe Baton] | 
  WinLevelMS [Maybe Baton] | 
  LoseLevelMS [Maybe Baton] | 
  NextLevelMS [Maybe Baton] | 
  RestartLevelMS [Maybe Baton]

-- Any data that we want to keep track of as the game progresses
-- Useful to put any constants that need accessing by multiple modes in here
-- Also used to save and load the game
data Baton = Baton 
  { currentLevel    :: LevelID
  , nextLevel       :: LevelID
  , randGen         :: [StdGen]
  , windowDim       :: (CInt, CInt)
  , prevLevelScores :: [Int]
  } deriving (Generic)
instance FromJSON Baton
instance ToJSON Baton

---

data GameOutputs 
  = PauseMenuOutputs PauseMenuOutputsData
  | StartMenuOutputs StartMenuOutputsData
  | IntroOutputs IntroOutputsData
  | PlayingOutputs PlayingOutputsData
  | WinScreenOutputs WinScreenOutputsData
  | LoseScreenOutputs LoseScreenOutputsData
  | EndScreenOutputs EndScreenOutputsData

data PauseMenuOutputsData = PauseMenuOutputsData
  { pauseMenuColOut :: Colour
  , unpauseEvent    :: Event ModeSwitch
  , saveEvent       :: Event (Int, Baton) 
  , loadEvent       :: Event ModeSwitch
  , quitPM          :: Bool
  }

data StartMenuOutputsData = StartMenuOutputsData
  { startMenuColOut :: Colour
  , startGameEvent  :: Event ModeSwitch
  , ballColOut      :: Colour
  , ballPos         :: V2 CInt
  , quitSM          :: Bool
  }

data IntroOutputsData = IntroOutputsData
  { introColOut     :: Colour
  , leaveIntroEvent :: Event ModeSwitch
  , quitIntro       :: Bool
  }

data PlayingOutputsData = PlayingOutputsData
  { windowDimPO       :: (CInt, CInt)
  , userBoundsPO      :: XYBounds
  , simBoundsPO       :: XYBounds
  , timeTotal         :: Time
  , timeJump          :: DTime
  , aliveGrid         :: Grid
  , checkGrid         :: Grid
  , totalAlive        :: Int
  , boxSize           :: CInt
  , viewOffsets       :: (CInt, CInt)
  , pendingBirths     :: Grid
  , pendingDeaths     :: Grid
  , switchEvent       :: Event ModeSwitch
  , score             :: Int
  , nextLifeBool      :: Bool
  , quitPlaying       :: Bool
  }

data WinScreenOutputsData = WinScreenOutputsData
  { winScreenColOut :: Colour
  , nextLevelEvent  :: Event ModeSwitch
  , quitWS          :: Bool
  }

data LoseScreenOutputsData = LoseScreenOutputsData
  { loseScreenColOut  :: Colour
  , retryLevelEvent   :: Event ModeSwitch
  , quitLS            :: Bool
  }

data EndScreenOutputsData = EndScreenOutputsData
  { endScreenColOut :: Colour
  , quitES          :: Bool
  }