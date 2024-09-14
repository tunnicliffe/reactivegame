module LifeHash 
  ( Grid 
  , unionGrid
  , differenceGrid
  , emptyGrid
  , gridToXYList
  , addNeighbours
  , simpleLifeBounded
  , insert
  , delete
  ) where

import Control.Monad  (join)
import Data.List (group, sort)

import qualified Data.HashMap.Strict as HM


type Grid = HM.HashMap (Int, Int) ()

unionGrid :: Grid -> Grid -> Grid
unionGrid = HM.union

differenceGrid :: Grid -> Grid -> Grid
differenceGrid = HM.difference

emptyGrid :: Grid
emptyGrid = HM.empty

addNeighbours :: Grid -> Grid
addNeighbours = fromListToGridHMWithNeighbours . gridToXYList 

fromListToGridHM :: [(Int, Int)] -> Grid
fromListToGridHM = HM.fromList . (`zip` repeat ())

fromListToGridHMWithNeighbours :: [(Int, Int)] -> Grid
fromListToGridHMWithNeighbours xs = fromListToGridHM (xs ++ join (map neighbours xs))

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

insert :: (Int, Int) -> Grid -> Grid
insert k = HM.insert k ()

delete :: (Int, Int) -> Grid -> Grid
delete = HM.delete

insertNullBounded :: (Int, Int, Int, Int) -> (Int, Int) -> Grid -> Grid
insertNullBounded (x_min, x_max, y_min, y_max) (x,y) gr = 
  if (x >= x_min) && (x <= x_max) && (y >= y_min) && (y <= y_max)
    then HM.insert (x,y) () gr
    else gr

deleteNullBounded :: (Int, Int, Int, Int) -> (Int, Int) -> Grid -> Grid
deleteNullBounded (x_min, x_max, y_min, y_max) (x,y) gr = 
  if (x >= x_min) && (x <= x_max) && (y >= y_min) && (y <= y_max)
    then HM.delete (x,y) gr
    else gr

gridToXYList :: Grid -> [(Int,Int)]
gridToXYList = map fst . HM.toList

simpleLifeBounded :: (Int, Int, Int, Int) -> (Grid, Grid) -> (Grid, Grid) 
  --alives BEFORE checks
  --Refuses to add out-of-bounds co-ords to checks or alives
simpleLifeBounded bounds (alive_grid, check_grid) = simpleLifeBoundedAcc bounds (gridToXYList check_grid) alive_grid alive_grid emptyGrid

simpleLifeBoundedAcc :: (Int, Int, Int, Int) -> [(Int, Int)] -> Grid -> Grid -> Grid -> (Grid, Grid)
simpleLifeBoundedAcc _ [] _ new_alive_grid new_check_grid = (new_alive_grid, new_check_grid)
simpleLifeBoundedAcc bounds ((x,y) : checks) old_alive_grid new_alive_grid new_check_grid = 
  let 
    is_alive       = (x,y) `HM.member` old_alive_grid
    locals         = neighbours (x,y)
    n_locals_alive = length . filter id $ map (`HM.member` old_alive_grid) locals
  in
    if is_alive
    then if (n_locals_alive == 2) || (n_locals_alive == 3)
      then simpleLifeBoundedAcc bounds checks old_alive_grid new_alive_grid new_check_grid
      else simpleLifeBoundedAcc bounds checks old_alive_grid (HM.delete (x,y) new_alive_grid) (foldr (insertNullBounded bounds) new_check_grid locals)
    else if n_locals_alive == 3
      then simpleLifeBoundedAcc bounds checks old_alive_grid (insertNullBounded bounds (x,y) new_alive_grid) (foldr (insertNullBounded bounds) new_check_grid locals)
      else simpleLifeBoundedAcc bounds checks old_alive_grid new_alive_grid new_check_grid
