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
  , shuffle
  , loss
  , lossForMove
  , lossForTile
  , trySolve
  , bruteSolve
  , SlidingPuzzle
  , bestLoss
  , bestMoves
  , SlidingPuzzleSolution
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

loss :: SlidingPuzzle -> Int
loss puzzle =
  let lossesForAll = map (lossForTile puzzle) [1 .. 15]
   in sum lossesForAll

lossForTile :: SlidingPuzzle -> Int -> Int
lossForTile puzzle tile =
  let slot = tileSlot puzzle tile
   in dist tile slot

lossForMove :: SlidingPuzzle -> Int -> Int
lossForMove puzzle tile =
  let slot = tileSlot puzzle tile
      empty = emptySlot puzzle
   in dist tile empty - dist tile slot

dist :: Int -> Int -> Int
dist tile slot =
  let tileRow = div tile 4
      slotRow = div slot 4
      tileCol = mod tile 4
      slotCol = mod slot 4
   in abs (tileRow - slotRow) + abs (tileCol - slotCol)

findSlot :: SlidingPuzzle -> Int -> Int
findSlot puzzle slot =
  let tiles = tileSlots puzzle
      found = elemIndex slot tiles
   in 1 + fromJust found

moveRandom :: RandomGen g => SlidingPuzzle -> g -> (SlidingPuzzle, Int, g)
moveRandom puzzle rgen =
  let possibleSlots = neighborsOfSlot (emptySlot puzzle)
      (idx, ngen) = randomR (0, length possibleSlots - 1) rgen
      chosenSlot = possibleSlots !! idx
      chosenTile = findSlot puzzle chosenSlot
      movedPuzzle = moveTile puzzle chosenTile
   in (movedPuzzle, chosenTile, ngen)

shuffle :: RandomGen g => SlidingPuzzle -> Int -> g -> (SlidingPuzzle, [Int], g)
shuffle puzzle moves rgen =
  let (movedPuzzle, tile, ngen) = moveRandom puzzle rgen
      (shuffledPuzzle, nextMoves, ggen) = shuffle movedPuzzle (moves - 1) ngen
      shuffled
        | moves == 1 = (movedPuzzle, [tile], ngen)
        | otherwise = (shuffledPuzzle, tile : nextMoves, ggen)
   in shuffled

trySolve :: SlidingPuzzle -> Int -> SlidingPuzzleSolution
trySolve puzzle = trySolveGivenLoss puzzle (loss puzzle)

trySolveGivenLoss :: SlidingPuzzle -> Int -> Int -> SlidingPuzzleSolution
trySolveGivenLoss puzzle givenLoss tries =
  let possibleSlots = neighborsOfSlot (emptySlot puzzle)
      possibleMoves = map (findSlot puzzle) possibleSlots
      possibleSolutions =
        map (trySolveWithMove puzzle givenLoss tries) possibleMoves
      best
        | tries < givenLoss || givenLoss == 0 = buildLeafSolution givenLoss
        | otherwise =
          let upper = buildLeafSolution 255
           in foldl compareSolutions upper possibleSolutions
   in best

trySolveWithMove :: SlidingPuzzle -> Int -> Int -> Int -> SlidingPuzzleSolution
trySolveWithMove puzzle givenLoss tries tile =
  let movedPuzzle = moveTile puzzle tile
      tryLoss = givenLoss + lossForMove puzzle tile
      solutionSoFar = trySolveGivenLoss movedPuzzle tryLoss (tries - 1)
   in SlidingPuzzleSolution
        { bestLoss = bestLoss solutionSoFar
        , bestMoves = tile : bestMoves solutionSoFar
        }

bruteSolve :: SlidingPuzzle -> SlidingPuzzleSolution
bruteSolve puzzle =
  let brute = map (trySolve puzzle) [0 .. 255]
      solution = find (\p -> bestLoss p == 0) brute
   in fromJust solution

data SlidingPuzzleSolution = SlidingPuzzleSolution
  { bestLoss  :: Int
  , bestMoves :: [Int]
  }

compareSolutions ::
     SlidingPuzzleSolution -> SlidingPuzzleSolution -> SlidingPuzzleSolution
compareSolutions first second =
  let choice
        | bestLoss first < bestLoss second = first
        | otherwise = second
   in choice

buildLeafSolution :: Int -> SlidingPuzzleSolution
buildLeafSolution bLoss =
  SlidingPuzzleSolution {bestLoss = bLoss, bestMoves = []}
