{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( loadDisplayResources
  , loadFramerateManager
  , initialInputs
  , detectDTimeAndInputs
  , loadBatons
  ) where

import FRP.Yampa        (DTime)
import SDL
import Data.Aeson       (decodeFileStrict)
import System.Random    (RandomGen, mkStdGen, split)
import System.Directory (doesFileExist)

import Types (UserInputs, nullUserInputs, GameConfigs, DisplayConfigs (..), DisplayResources (..), Baton (..), LevelID (..))
import Input (detectInputs)

import qualified SDL.Framerate as Framerate
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import qualified Data.HashMap.Strict as HM

---

loadDisplayResources :: DisplayConfigs -> IO DisplayResources
loadDisplayResources dc = do

  wind <- createWindow (windowName dc) defaultWindow
  rend <- createRenderer wind (-1) defaultRenderer
  rendererDrawBlendMode rend $= BlendAlphaBlend

  Font.initialize
  
  gugiFontCounter <- Font.load (fontPath dc) (counterFontSize dc)
  scoreSurface <- Font.blended gugiFontCounter (counterCol dc) "Score: "
  scoreT <- createTextureFromSurface rend scoreSurface
  freeSurface scoreSurface
  timeSurface <- Font.blended gugiFontCounter (counterCol dc) "Time: "
  timeT <- createTextureFromSurface rend timeSurface
  freeSurface timeSurface
  fpsSurface <- Font.blended gugiFontCounter (counterCol dc) "FPS: "
  fpsT <- createTextureFromSurface rend fpsSurface
  freeSurface fpsSurface
  digitSurfaces <- mapM (Font.blendedGlyph gugiFontCounter (counterCol dc)) "0123456789."
  digitTs <- mapM (createTextureFromSurface rend) digitSurfaces
  mapM_ freeSurface digitSurfaces
  Font.free gugiFontCounter

  gugiFontText <- Font.load "fonts/Gugi-Regular.ttf" (textFontSize dc)
  let charList = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".,+=!?_-()[]{}<>;: "
  charSurfaces <- mapM (Font.blendedGlyph gugiFontText (textCol dc)) charList
  charTs <- mapM (createTextureFromSurface rend) charSurfaces
  mapM_ freeSurface charSurfaces
  let charTMap = HM.fromList (zip charList charTs) 
  Font.free gugiFontText
  
  Font.quit

  Mixer.openAudio Mixer.defaultAudio 512 -- The ChunkSize I am using here is totally arbitrary
  testChunk <- Mixer.load "sounds/447__tictacshutup__prac-snare.wav"

  pure $ DisplayResources wind rend scoreT timeT fpsT digitTs charTMap testChunk

loadFramerateManager :: DisplayConfigs -> IO Framerate.Manager 
loadFramerateManager displayConf = do 
  framerateManager <- Framerate.manager 
  Framerate.set framerateManager (framerateLimit displayConf)
  pure framerateManager

initialInputs :: Framerate.Manager -> IO UserInputs
initialInputs framerateManager = do 
  Framerate.delay framerateManager
  pure nullUserInputs

detectDTimeAndInputs :: Framerate.Manager 
                     -> Bool -- Blocking argument that reactimate never checks anyway
                     -> IO (DTime, Maybe UserInputs)
detectDTimeAndInputs framerateManager _ = do 
  dtMilliseconds <- Framerate.delay framerateManager
  maybeInputs    <- detectInputs
  -- print (fromMaybe maybeInputs)
  pure (fromIntegral dtMilliseconds, maybeInputs)

loadBatons :: DisplayConfigs -> Int -> IO [Maybe Baton]
loadBatons dc seed = 
  do 
    loadedBatons <- traverse loadBaton ["saves/" ++ [n] ++ ".sav" | n <- "123456789"]
    let b0 = Just $ initialBaton dc seed
    return (b0 : loadedBatons)

loadBaton :: FilePath -> IO (Maybe Baton)
loadBaton fp = do 
  x <- doesFileExist fp 
  if x then decodeFileStrict fp else return Nothing

initialBaton :: DisplayConfigs -> Int -> Baton 
initialBaton dc seed = Baton StartScreen Level1 (splitN 10 $ mkStdGen seed) (initialWindowDim dc) []

--

split3 :: (RandomGen g) => g -> (g, g, g)
split3 g = (g1, g2, g3) where
  (g0, g1) = split g 
  (g2, g3) = split g0

splitN :: (RandomGen g) => Int -> g -> [g]
splitN n g | n < 1 = error "splitN: n < 1."
splitN 1 g = [g]
splitN n g = let (g', g'') = split g in g' : splitN (n-1) g''

{-
splitInf :: (RandomGen g) => g -> [g]
splitInf g = 
  let (g', g'') = split g 
  in  g' : splitInf g''
-- Don't use this, as the [StdGen] will be written to file

testImage renderer file_path = do
  texture <- Image.loadTexture renderer file_path
  copy renderer texture Nothing (Just $ Rectangle (P (V2 0 0)) (V2 635 476))
  present renderer
  wait <- getLine
  pure ()
-}
