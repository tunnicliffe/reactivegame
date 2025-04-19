{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types 
  ( XYBounds
  , Colour
  , Grid

  , DisplayConfig(..)
  , DisplayResources(..)

  , UserInputs(..)
  , nullUserInputs
  , InputKey(..)
  , keycodeToIK
  , InputState(..)
  , InputAction(..)

  , LevelType(..)
  , ModeSwitch(..)
  , Baton(..)

  , GameOutputs(..)
  , PauseMenuOutputsData(..)
  , StartMenuOutputsData(..)
  , IntroOutputsData(..)
  , PlayingOutputsData(..)
  , WinScreenOutputsData(..)
  , LoseScreenOutputsData(..)
  , EndScreenOutputsData(..)
  ) where

import ClassyPrelude

import FRP.Yampa            (Event(..), Time, DTime, SF)
import SDL                  (V2(..), V3(..), V4(..), Point(..), Texture, Renderer, Window)
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
import System.Random.Internal (StdGen(..))
import Text.Read (read)

import InputKey (InputKey (..), keycodeToIK)


-- Useful instances

instance RealFloat a => VectorSpace (V2 a) a where
  zeroVector = V2 0 0
  a *^ (V2 x y) = V2 (a * x) (a * y)
  (V2 x y) ^/ a = V2 (x / a) (y / a)
  negateVector (V2 x y) = V2 (-x) (-y)
  (V2 x1 y1) ^+^ (V2 x2 y2) = V2 (x1+x2) (y1+y2)
  (V2 x1 y1) ^-^ (V2 x2 y2) = V2 (x1-x2) (y1-y2)
  (V2 x1 y1) `dot` (V2 x2 y2) = x1 * x2 + y1 * y2

instance RealFloat a => VectorSpace (V3 a) a where
  zeroVector = V3 0 0 0
  a *^ (V3 x y z) = V3 (a * x) (a * y) (a * z)
  (V3 x y z) ^/ a = V3 (x / a) (y / a) (z / a)
  negateVector (V3 x y z) = V3 (-x) (-y) (-z)
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

deriving instance Read StdGen
instance FromJSON StdGen where
  parseJSON v = read <$> parseJSON v
instance ToJSON StdGen where
  toJSON = toJSON . show 

---

type XYBounds = (Int, Int, Int, Int) -- (x_min, x_max, y_min, y_max)

type Colour = V4 Word8

type Grid = HashMap (Int, Int) ()

---

-- Immuatable variables that can only be accessed by the displayFunction

-- DisplayConfig is parsed at runtime from a config file 
data DisplayConfig = DisplayConfig
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
instance FromJSON DisplayConfig

-- DisplayResources are loaded at runtime using some values from the DisplayConfig
data DisplayResources = DisplayResources 
  { window            :: Window
  , renderer          :: Renderer 
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
  , quitBool          :: Bool -- TODO: action on this in switchModeDetectSF
  }

nullUserInputs = UserInputs 
  { keyboardInputs    = []
  , mouseLeftInputs   = []
  , mouseRightInputs  = []
  , quitBool          = False
  }

data InputState = Press | Hold | Release 
  deriving (Eq)

-- | Type for what action the InputKey is causing
-- Each of our Configs types in GameConfigs have a Map InputKey InputAction
-- We can use this to re-bind inputs to different actions
data InputAction
  = Pause
  | Unpause
  | Quit
  | StartGame
  | LeaveIntro
  | IncreaseBoxSize
  | DecreaseBoxSize
  | MoveViewRight
  | MoveViewLeft
  | MoveViewDown
  | MoveViewUp
  | EnactCells
  | IncreaseLifeDelay
  | DecreaseLifeDelay
  | NextLife
  | NextLevel
  | RetryLevel
  deriving (Generic, Eq)
instance FromJSON InputAction
instance Hashable InputAction

---

data LevelType
  = StartScreen
  | Level1
  | Level2
  deriving (Generic, Eq, Show, Read, Enum)
instance Hashable LevelType
instance FromJSON LevelType
instance FromJSONKey LevelType
instance ToJSON LevelType

-- Switching mode means changing the (SF UserInputs GameOutputs) currently in use
-- This is used to switch between the modes of the game whilst retaining the Baton
data ModeSwitch
  = MSPauseLevel Baton
  | MSUnpauseLevel
  | MSLoadLevel Baton
  | MSLeaveIntro Baton
  | MSWinLevel Baton
  | MSLoseLevel Baton
  | MSNextLevel Baton
  | MSRestartLevel Baton

-- Any data that we want to keep track of as the game progresses between modes
-- Probably what we want to use later in saving and loading
data Baton = Baton 
  { currentLevel    :: LevelType
  , nextLevel       :: LevelType
  , randGen         :: StdGen
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
  --, saveEvent       :: Event (Int, Baton) 
  --, loadEvent       :: Event ModeSwitch
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
