module Input 
  ( detectInputs
  , pressBool
  , releaseBool
  , quantifyInputPair
  , quantifyInputPairSF
  , clicksPressedOrHeld
  , keyRemapToHM
  ) where

import SDL

import FRP.Yampa (SF, arrPrim)
import Foreign.C.Types (CInt)
import Data.Int (Int32)
import Data.List (foldl')
--import Control.Monad (unless)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

import Types (UserInputs (..), nullUserInputs, InputState (..), InputAction, InputKey, keycodeToIK)

detectInputs :: IO (Maybe UserInputs)
detectInputs = do
  rawEvents <- pollEvents
  --unless (null rawEvents) (print rawEvents)
  let userInputs = foldl' recordEvent nullUserInputs $ map eventPayload rawEvents 
  pure $ Just userInputs

---

recordEvent :: UserInputs -> EventPayload -> UserInputs
recordEvent acc pl = case pl of
  KeyboardEvent (KeyboardEventData _ inputMotion hold (Keysym _ keycode _)) -> 
    addKeyInput acc keycode $ inputState inputMotion hold
  MouseButtonEvent (MouseButtonEventData _ inputMotion _ mouseButton _ pos) -> 
    addMouseInput acc mouseButton (pos, inputState inputMotion False)
  MouseMotionEvent (MouseMotionEventData _ _ [mouseButton] pos _) ->
    addMouseInput acc mouseButton (pos, Hold)
  QuitEvent -> 
    acc {quitBool = True}
  _ ->
    acc

addKeyInput :: UserInputs -> Keycode -> InputState -> UserInputs
addKeyInput gi kc ks = gi {keyboardInputs = (keycodeToIK kc, ks) : (keyboardInputs gi)}

inputState :: InputMotion -> Bool -> InputState 
inputState Pressed hold = if hold then Hold else Press
inputState _       _    = Release 

addMouseInput :: UserInputs -> MouseButton -> (Point V2 Int32, InputState) -> UserInputs
addMouseInput gi ButtonLeft  x = gi {mouseLeftInputs  = x : (mouseLeftInputs  gi)}
addMouseInput gi ButtonRight x = gi {mouseRightInputs = x : (mouseRightInputs gi)}

---

pressBool :: InputAction -> HM.HashMap InputAction InputState -> Bool 
pressBool ia hm = case HM.lookup ia hm of
  Just Press -> True
  _          -> False

releaseBool :: InputAction -> HM.HashMap InputAction InputState -> Bool 
releaseBool ia hm = case HM.lookup ia hm of
  Just Release -> True
  _            -> False

quantifyInputPair :: (a, a, a, a, a) -> (Maybe InputState, Maybe InputState) -> a 
quantifyInputPair (vLow, low, mid, high, vHigh) (Just Hold,  Nothing) = vHigh
quantifyInputPair (vLow, low, mid, high, vHigh) (Just Press, Nothing) = high
quantifyInputPair (vLow, low, mid, high, vHigh) (Nothing, Just Hold)  = vLow
quantifyInputPair (vLow, low, mid, high, vHigh) (Nothing, Just Press) = low 
quantifyInputPair (vLow, low, mid, high, vHigh) _                     = mid

quantifyInputPairSF :: (a, a, a, a, a) -> SF (Maybe InputState, Maybe InputState) a
quantifyInputPairSF levels = arrPrim $ quantifyInputPair levels

---

clicksPressedOrHeld :: UserInputs -> MouseButton -> [Point V2 Int32]
clicksPressedOrHeld ri ButtonLeft  = fst . unzip $ filter clickCheckPH $ mouseLeftInputs  ri 
clicksPressedOrHeld ri ButtonRight = fst . unzip $ filter clickCheckPH $ mouseRightInputs ri 

clickCheckPH :: (Point V2 Int32, InputState) -> Bool 
clickCheckPH (_, Release) = False
clickCheckPH _            = True

---

keyRemap :: (Hashable k1, Eq k1) => [(k1, v)] -> HM.HashMap k1 k2 -> [(k2, v)]
keyRemap ((k1, v) : xs) hm = case HM.lookup k1 hm of
  Just k2 -> (k2, v) : keyRemap xs hm 
  Nothing -> keyRemap xs hm
keyRemap [] _ = []

keyRemapToHM :: UserInputs -> HM.HashMap InputKey InputAction -> HM.HashMap InputAction InputState 
keyRemapToHM userInputs hm = HM.fromList $ keyRemap (keyboardInputs userInputs) hm