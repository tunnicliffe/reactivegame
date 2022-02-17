import qualified Data.HashMap.Strict as HM
import SDL (V4 (V4))
import Types (InputKey (KeyRETURN, KeyESCAPE), InputAction (Pause, Quit))
import Data.Yaml (encodeFile)
import Data.Aeson (ToJSON, ToJSONKey)

instance ToJSON InputAction
instance ToJSON InputKey
instance ToJSONKey InputKey
instance (ToJSON a) => ToJSON (V4 a)

testHM :: HM.HashMap InputKey InputAction
testHM = HM.fromList [(KeyRETURN, Pause), (KeyESCAPE, Quit)]

testV4 :: V4 Int 
testV4 = V4 1 2 3 4

main :: IO ()
main = encodeFile "test/test.yaml" [testV4, testV4]
