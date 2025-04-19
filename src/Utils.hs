{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Utils
  ( loadDisplayResources
  , loadFramerateManager
  , initialInputs
  , detectDTimeAndInputs
  , initialBaton
  ) where

import ClassyPrelude
import Types

import Data.Aeson (decodeFileStrict)
import FRP.Yampa (DTime)
import Input (detectInputs)
import System.Directory (doesFileExist)
import System.Random (RandomGen, mkStdGen, split)

import qualified Data.HashMap.Strict as HM
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Framerate as Framerate
import qualified SDL.Mixer as Mixer

---

loadDisplayResources :: DisplayConfig -> IO DisplayResources
loadDisplayResources DisplayConfig{..} = do

  window <- SDL.createWindow windowName SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend

  Font.initialize
  
  gugiFontCounter <- Font.load fontPath counterFontSize
  scoreSurface <- Font.blended gugiFontCounter counterCol "Score: "
  scoreTexture <- SDL.createTextureFromSurface renderer scoreSurface
  SDL.freeSurface scoreSurface
  timeSurface <- Font.blended gugiFontCounter counterCol "Time: "
  timeTexture <- SDL.createTextureFromSurface renderer timeSurface
  SDL.freeSurface timeSurface
  fpsSurface <- Font.blended gugiFontCounter counterCol "FPS: "
  fpsTexture <- SDL.createTextureFromSurface renderer fpsSurface
  SDL.freeSurface fpsSurface
  digitSurfaces <- mapM (Font.blendedGlyph gugiFontCounter counterCol) "0123456789."
  digitTextures <- mapM (SDL.createTextureFromSurface renderer) digitSurfaces
  mapM_ SDL.freeSurface digitSurfaces
  Font.free gugiFontCounter

  gugiFontText <- Font.load "fonts/Gugi-Regular.ttf" textFontSize
  let charList = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".,+=!?_-()[]{}<>;: "
  charSurfaces <- mapM (Font.blendedGlyph gugiFontText textCol) charList
  charTs <- mapM (SDL.createTextureFromSurface renderer) charSurfaces
  mapM_ SDL.freeSurface charSurfaces
  let charTextureMap = HM.fromList (zip charList charTs) 
  Font.free gugiFontText
  Font.quit

  Mixer.openAudio Mixer.defaultAudio 512
  audioChunk <- Mixer.load "sounds/447__tictacshutup__prac-snare.wav"

  pure $ DisplayResources{..}

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

{-
testImage renderer file_path = do
  texture <- Image.loadTexture renderer file_path
  copy renderer texture Nothing (Just $ Rectangle (P (V2 0 0)) (V2 635 476))
  present renderer
  wait <- getLine
  pure ()
-}
