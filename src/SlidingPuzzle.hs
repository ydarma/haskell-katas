module SlidingPuzzle
  ( buildPuzzle
  , tileSlots
  , tileSlot
  , emptySlot
  , neighborsOfSlot
  , isNeighborOfSlot
  , moveTile
  , moveTiles
  , moveRandom
  , loss
  , lossForTile
  , SlidingPuzzle
  ) where

import           Data.List
import           Data.Maybe
import           System.Random

data SlidingPuzzle = SlidingPuzzle
  { emptySlot :: Int
  , tileSlots :: [Int]
  }

buildPuzzle :: SlidingPuzzle
buildPuzzle = SlidingPuzzle {emptySlot = 0, tileSlots = [1 .. 15]}

tileSlot :: SlidingPuzzle -> Int -> Int
tileSlot puzzle tile =
  if tile > 0 && tile < 16
    then tileSlots puzzle !! (tile - 1)
    else error "invalid tile"

neighborsOfSlot :: Int -> [Int]
neighborsOfSlot 0  = [1, 4]
neighborsOfSlot 1  = [0, 2, 5]
neighborsOfSlot 2  = [1, 3, 6]
neighborsOfSlot 3  = [2, 7]
neighborsOfSlot 4  = [0, 5, 8]
neighborsOfSlot 5  = [1, 4, 6, 9]
neighborsOfSlot 6  = [2, 5, 7, 10]
neighborsOfSlot 7  = [3, 6, 11]
neighborsOfSlot 8  = [4, 9, 12]
neighborsOfSlot 9  = [5, 8, 10, 13]
neighborsOfSlot 10 = [6, 9, 11, 14]
neighborsOfSlot 11 = [7, 10, 15]
neighborsOfSlot 12 = [8, 13]
neighborsOfSlot 13 = [9, 12, 14]
neighborsOfSlot 14 = [10, 13, 15]
neighborsOfSlot 15 = [11, 14]
neighborsOfSlot _  = error "Slot does not exist"

isNeighborOfSlot :: Int -> Int -> Bool
isNeighborOfSlot slot otherSlot =
  let neighbors = neighborsOfSlot slot
   in elem otherSlot neighbors

moveTile :: SlidingPuzzle -> Int -> SlidingPuzzle
moveTile puzzle tile =
  let empty = emptySlot puzzle
      slot = tileSlot puzzle tile
      newSlots =
        if isNeighborOfSlot slot empty
          then changeTileSlot (tileSlots puzzle) tile empty
          else error "invalid move"
   in SlidingPuzzle {emptySlot = slot, tileSlots = newSlots}

moveTiles :: SlidingPuzzle -> [Int] -> SlidingPuzzle
moveTiles = foldl moveTile

changeTileSlot :: [Int] -> Int -> Int -> [Int]
changeTileSlot tiles tile slot =
  let (left, _:right) = splitAt (tile - 1) tiles
   in left ++ slot : right

lossForTile :: SlidingPuzzle -> Int -> Int
lossForTile puzzle tile =
  let slot = tileSlot puzzle tile
      tileRow = div tile 4
      slotRow = div slot 4
      tileCol = mod tile 4
      slotCol = mod slot 4
   in abs (tileRow - slotRow) + abs (tileCol - slotCol)

loss :: SlidingPuzzle -> Int
loss puzzle =
  let lossesForAll = map (lossForTile puzzle) [1 .. 15]
   in sum lossesForAll

moveRandom :: RandomGen g => SlidingPuzzle -> g -> (SlidingPuzzle, Int, g)
moveRandom puzzle rgen =
  let possibleSlots = neighborsOfSlot (emptySlot puzzle)
      (idx, ngen) = randomR (0, length possibleSlots - 1) rgen
      chosenSlot = possibleSlots !! idx
      chosenTile = 1 + (fromJust . elemIndex chosenSlot $ tileSlots puzzle)
      movedPuzzle = moveTile puzzle chosenTile
   in (movedPuzzle, chosenTile, ngen)
