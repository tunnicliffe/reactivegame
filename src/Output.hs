{-# LANGUAGE RecordWildCards #-}

module Output (displayFunction) where

import SDL

import Control.Monad (when)
import Data.Aeson (encodeFile)
import Data.Maybe (fromMaybe)
import Data.Vector.Storable (fromList)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import FRP.Yampa (Time, DTime, fromEvent, isEvent)
import LifeHash (gridToXYList)
import SDL.Mixer (play)
import Types ( Colour, XYBounds, DisplayConfig(..), DisplayResources(..), GameOutputs(..), PauseMenuOutputsData(..), StartMenuOutputsData(..), IntroOutputsData(..), PlayingOutputsData(..), WinScreenOutputsData(..), LoseScreenOutputsData(..), EndScreenOutputsData(..))

import qualified Data.HashMap.Strict as HM


displayFunction
  :: DisplayConfig
  -> DisplayResources
  -> Bool -- have GameOuputs changed (`reactimate` seems to skip this check anyway?)
  -> GameOutputs 
  -> IO Bool -- should `reactimate` end

displayFunction _ _ False _ = pure False

displayFunction DisplayConfig{..} DisplayResources{..} True (PlayingOutputs PlayingOutputsData{..}) = do 
  -- Clear screen
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  -- Draw grid and cells
  drawRectFromBounds renderer bgCol boxSize viewOffsets simBoundsPO
  drawRectFromBounds renderer userAreaCol boxSize viewOffsets userBoundsPO
  drawGridLines renderer lineCol windowDimPO boxSize viewOffsets
  drawSquaresFromCoOrds renderer checkCol boxSize viewOffsets (gridToXYList checkGrid)
  drawSquaresFromCoOrds renderer aliveCol boxSize viewOffsets (gridToXYList aliveGrid)
  drawSquaresFromCoOrds renderer pendingBirthCol boxSize viewOffsets (gridToXYList pendingBirths)
  drawSquaresFromCoOrds renderer pendingDeathCol boxSize viewOffsets (gridToXYList pendingDeaths)
  -- Draw counters
  drawCounter renderer scoreOffset scoreTexture digitTextures (digits score)
  drawCounter renderer timeOffset timeTexture digitTextures (cleanTimeDigits timeTotal)
  drawCounter renderer fpsOffset fpsTexture digitTextures (digits $ dtToFPS timeJump)
  -- Finish
  present renderer
  when nextLifeBool (play audioChunk) -- audio test
  pure quitPlaying

displayFunction dc dr True (PauseMenuOutputs pmo) = do
  --rendererDrawColor rrnd $= menuColOut pmo
  --clear rend
  --present rend
  --let  (slot, baton) = fromEvent $ saveEvent pmo
  --when (isEvent (saveEvent pmo)) $ encodeFile ("saves/" ++ show slot ++ ".sav") baton
  pure $ quitPM pmo

displayFunction dc dr True (StartMenuOutputs smo) = do
  let rend = renderer dr
  rendererDrawColor rend $= startMenuColOut smo
  clear rend
  drawString rend (charTextureMap dr) (V2 10 10) ("Start Menu " ++ show (startMenuColOut smo))
  rendererDrawColor rend $= ballColOut smo 
  fillRect rend $ Just $ Rectangle (P $ ballPos smo) (V2 10 10)
  present rend
  pure $ quitSM smo

displayFunction dc dr True (IntroOutputs io) = do
  let rend = renderer dr
  rendererDrawColor rend $= introColOut io
  clear rend
  drawString rend (charTextureMap dr) (V2 10 10) "Introduction"
  present rend
  pure $ quitIntro io

displayFunction dc dr True (WinScreenOutputs wso) = do
  let rend = renderer dr
  rendererDrawColor rend $= winScreenColOut wso
  clear rend
  drawString rend (charTextureMap dr) (V2 10 10) "Win Screen"
  present rend
  pure $ quitWS wso 

displayFunction dc dr True (LoseScreenOutputs lso) = do
  let rend = renderer dr
  rendererDrawColor rend $= loseScreenColOut lso
  clear rend
  drawString rend (charTextureMap dr) (V2 10 10) "Lose Screen"
  present rend
  pure $ quitLS lso 

displayFunction dc dr True (EndScreenOutputs eso) = do 
  let rend = renderer dr
  rendererDrawColor rend $= endScreenColOut eso
  clear rend
  drawString rend (charTextureMap dr) (V2 10 10) "End Screen"
  present rend
  pure $ quitES eso


---

drawGridLines :: Renderer -> Colour -> (CInt, CInt) -> CInt -> (CInt, CInt) -> IO ()
drawGridLines rend lineCol (width, height) gap (xOffset, yOffset) = do
  rendererDrawColor rend $= lineCol
  let (xShift, yShift) = (gap - (xOffset `mod` gap), gap - (yOffset `mod` gap))
  drawSeparateLines rend $ zip (map (`V2` 0) [xShift, (xShift+gap) .. width])  (map (`V2` height) [xShift, (xShift+gap) .. width])
  drawSeparateLines rend $ zip (map (V2 0)   [yShift, (yShift+gap) .. height]) (map (V2 width)    [yShift, (yShift+gap) .. height])

drawSeparateLines :: Renderer -> [(V2 CInt, V2 CInt)] -> IO ()
drawSeparateLines _    []         = pure ()
drawSeparateLines rend ((x,y):ls) = drawLine rend (P x) (P y) >> drawSeparateLines rend ls

drawSquaresFromCoOrds :: Renderer -> Colour -> CInt -> (CInt, CInt) -> [(Int, Int)] -> IO ()
drawSquaresFromCoOrds rend col size (xOffset, yOffset) coOrds = do
  let
    makeBox (x, y) = Rectangle (P (V2 (fromIntegral x * size - xOffset + 1) (fromIntegral y * size - yOffset + 1))) (V2 (size - 1) (size - 1))
    rectVec = fromList $ map makeBox coOrds
  rendererDrawColor rend $= col
  fillRects rend rectVec
  -- Doesn't check squares are within view. Potentially wasteful?
  -- could be a little cleaner looking..

drawRectFromBounds :: Renderer -> Colour -> CInt -> (CInt, CInt) -> XYBounds -> IO ()
drawRectFromBounds rend col size (xOffset, yOffset) (x_min, x_max, y_min, y_max) = do
  let rect = Rectangle (P (V2 (fromIntegral x_min * size - xOffset + 1) (fromIntegral y_min * size - yOffset + 1))) (V2 (fromIntegral (x_max - x_min) * size) (fromIntegral (y_max - y_min) * size))
  rendererDrawColor rend $= col
  fillRect rend (Just rect)

drawCounter :: Renderer -> V2 CInt -> Texture -> [Texture] -> [Int] -> IO ()
drawCounter rend offset ct dts ns = do
  tWidth <- textureWidth <$> queryTexture ct
  copySimple rend ct offset
  let newOffset = offset + V2 tWidth 0
  drawDigits rend dts newOffset ns

digits :: (Num a, Show a) => a -> [Int] 
digits n = map (readWithDot . (: [])) $ show n

drawDigits :: Renderer -> [Texture] -> V2 CInt -> [Int] -> IO ()
drawDigits rend dts offset (n:ns) = do
  digitTWidth <- textureWidth <$> queryTexture (head dts)
  copySimple rend (dts !! n) offset
  let newOffset = offset + V2 digitTWidth 0
  drawDigits rend dts newOffset ns
drawDigits _ _ _ [] = pure ()

copySimple :: Renderer -> Texture -> V2 CInt -> IO ()
copySimple rend t offset = do 
  tDims <- queryTextureDims t
  copy rend t Nothing $ Just $ Rectangle (P offset) tDims
  -- copy the whole texture identically

queryTextureDims :: Texture -> IO (V2 CInt)
queryTextureDims t = do 
  tInfo <- queryTexture t
  pure $ V2 (textureWidth tInfo) (textureHeight tInfo)

readWithDot :: String -> Int
readWithDot "." = 10
readWithDot n   = read n

cleanTimeDigitsInternal :: [Int] -> [Int]
cleanTimeDigitsInternal (10 : x : y : _) = [10, x, y]
cleanTimeDigitsInternal (x : xs)         = x : cleanTimeDigitsInternal xs
cleanTimeDigitsInternal []               = []
-- Shows two significant figures

cleanTimeDigits :: Time -> [Int]
cleanTimeDigits t = cleanTimeDigitsInternal $ map (readWithDot . (: [])) $ show t

dtToFPS :: DTime -> Int 
dtToFPS 0  = 999
dtToFPS dt = round $ 1000 / dt

drawString :: Renderer -> HM.HashMap Char Texture -> V2 CInt -> String -> IO ()
drawString _ _ _ [] = pure ()
drawString rend ctm offset (c:cs) = do 
  let maybeGlyph = HM.lookup c ctm
  let glyph = fromMaybe (error $ c : " character not in charTextureMap") maybeGlyph
  glyphWidth <- textureWidth <$> queryTexture glyph
  copySimple rend glyph offset
  let newOffset = offset + V2 glyphWidth 0
  drawString rend ctm newOffset cs 
