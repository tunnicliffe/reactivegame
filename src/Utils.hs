{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Utils
  ( loadDisplayResources
  , loadFramerateManager
  , initialInputs
  , detectDTimeAndInputs
  , initialBaton
  , splitN
  ) where

import ClassyPrelude

import Data.Aeson (decodeFileStrict)
import FRP.Yampa (DTime)
import Input (detectInputs)
import System.Directory (doesFileExist)
import System.Random (RandomGen, mkStdGen, split)
import Types (UserInputs, nullUserInputs, DisplayConfig(..), DisplayResources (DisplayResources), Baton (Baton), LevelType (StartScreen, Level1))

import qualified Data.HashMap.Strict as HM
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Framerate as Framerate
import qualified SDL.Mixer as Mixer

---

loadDisplayResources :: DisplayConfig -> IO DisplayResources
loadDisplayResources DisplayConfig{..} = do

  wind <- SDL.createWindow windowName SDL.defaultWindow
  rend <- SDL.createRenderer wind (-1) SDL.defaultRenderer
  SDL.rendererDrawBlendMode rend SDL.$= SDL.BlendAlphaBlend

  Font.initialize
  
  gugiFontCounter <- Font.load fontPath counterFontSize
  scoreSurface <- Font.blended gugiFontCounter counterCol "Score: "
  scoreT <- SDL.createTextureFromSurface rend scoreSurface
  SDL.freeSurface scoreSurface
  timeSurface <- Font.blended gugiFontCounter counterCol "Time: "
  timeT <- SDL.createTextureFromSurface rend timeSurface
  SDL.freeSurface timeSurface
  fpsSurface <- Font.blended gugiFontCounter counterCol "FPS: "
  fpsT <- SDL.createTextureFromSurface rend fpsSurface
  SDL.freeSurface fpsSurface
  digitSurfaces <- mapM (Font.blendedGlyph gugiFontCounter counterCol) "0123456789."
  digitTs <- mapM (SDL.createTextureFromSurface rend) digitSurfaces
  mapM_ SDL.freeSurface digitSurfaces
  Font.free gugiFontCounter

  gugiFontText <- Font.load "fonts/Gugi-Regular.ttf" textFontSize
  let charList = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".,+=!?_-()[]{}<>;: "
  charSurfaces <- mapM (Font.blendedGlyph gugiFontText textCol) charList
  charTs <- mapM (SDL.createTextureFromSurface rend) charSurfaces
  mapM_ SDL.freeSurface charSurfaces
  let charTMap = HM.fromList (zip charList charTs) 
  Font.free gugiFontText
  
  Font.quit

  Mixer.openAudio Mixer.defaultAudio 512
  testChunk <- Mixer.load "sounds/447__tictacshutup__prac-snare.wav"

  pure $ DisplayResources wind rend scoreT timeT fpsT digitTs charTMap testChunk

loadFramerateManager :: DisplayConfig -> IO Framerate.Manager 
loadFramerateManager dc = do 
  framerateManager <- Framerate.manager 
  Framerate.set framerateManager $ framerateLimit dc
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

initialBaton :: DisplayConfig -> Int -> Baton 
initialBaton dc seed = Baton StartScreen Level1 (mkStdGen seed) (initialWindowDim dc) []

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
