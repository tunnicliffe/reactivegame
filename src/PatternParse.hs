module PatternParse where

import Data.List.Split (splitOn)

-- Supports RLE, Plaintext (.cells), and Life 1.06 (.lif) BUT NOT 1.05

patternParse :: String -> String -> [(Int,Int)]
patternParse file_path file_contents = 
  case reverse . take 3 $ reverse file_path of 
    "lif" -> lifeSixStringToList file_contents
    "lls" -> plaintextStringToList file_contents
    "rle" -> rleStringToList file_contents
    _     -> error ("Bad filetype passed into patternParse: " ++ show file_path)

lifeSixStringToList :: String -> [(Int, Int)]
lifeSixStringToList raw = 
  let
    coord_strings = (init . tail) $ splitOn "\r\n" raw
    coord_string_pairs = map (splitOn " ") coord_strings
    tupleFunc [x, y] = (read x, read y)
  in
    map tupleFunc coord_string_pairs

plaintextStringToList :: String -> [(Int, Int)]
plaintextStringToList to_do = []

rleStringToList :: String -> [(Int, Int)]
rleStringToList to_do = []