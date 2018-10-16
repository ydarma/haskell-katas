module SlidingPuzzle where

import           Data.List
import           Data.Maybe
import           System.Random

data SlidingPuzzle = SlidingPuzzle
  { emptySlot :: Int
  , tileSlots :: [Int]
  }
  deriving Eq

makePuzzle :: SlidingPuzzle
makePuzzle = SlidingPuzzle {emptySlot = 0, tileSlots = [1 .. 15]}

tileSlot :: SlidingPuzzle -> Int -> Int
tileSlot puzzle tile
  | tile > 0 && tile < 16 = tileSlots puzzle !! (tile - 1)
  | otherwise = error "invalid tile"

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

moveTiles :: SlidingPuzzle -> [Int] -> SlidingPuzzle
moveTiles = foldl moveTile

moveTile :: SlidingPuzzle -> Int -> SlidingPuzzle
moveTile puzzle tile =
  let empty = emptySlot puzzle
      slot = tileSlot puzzle tile
      newSlots
        | isNeighborOfSlot slot empty =
          let currentSlots = tileSlots puzzle
           in changeTileSlot currentSlots tile empty
        | otherwise = error "invalid move"
   in SlidingPuzzle {emptySlot = slot, tileSlots = newSlots}

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
shuffle puzzle 0 rgen = (puzzle, [], rgen)
shuffle puzzle moves rgen =
  let (movedPuzzle, tile, ngen) = moveRandom puzzle rgen
      (shuffledPuzzle, nextMoves, ggen) = shuffle movedPuzzle (moves - 1) ngen
   in (shuffledPuzzle, tile : nextMoves, ggen)

bruteSolve :: SlidingPuzzle -> SlidingPuzzleSolution
bruteSolve puzzle =
   ida puzzle SlidingPuzzleSolution { bestLoss = loss puzzle, bestMoves = [] } 0

ida :: SlidingPuzzle -> SlidingPuzzleSolution -> Int -> SlidingPuzzleSolution
ida puzzle solution@SlidingPuzzleSolution { bestLoss = 0 } _ = solution
ida puzzle solution triesSoFar =
  let minimalTries = length (bestMoves solution) + bestLoss solution
      deeperTries
        | triesSoFar < minimalTries = minimalTries
        | otherwise = triesSoFar + 1
      deeperSolution = trySolve puzzle deeperTries
   in ida puzzle deeperSolution deeperTries

trySolve :: SlidingPuzzle -> Int -> SlidingPuzzleSolution
trySolve puzzle = trySolveGivenLoss [puzzle] (loss puzzle)

trySolveGivenLoss :: [SlidingPuzzle] -> Int -> Int -> SlidingPuzzleSolution
trySolveGivenLoss path@(puzzle:parents) givenLoss tries
  | tries < givenLoss || givenLoss == 0 = makeLeafSlidingPuzzleSolution givenLoss
  | otherwise =
    let possibleSlots = neighborsOfSlot (emptySlot puzzle)
        possibleMoves = map (findSlot puzzle) possibleSlots
        solver = trySolveWithMove path givenLoss tries
        allSolutions = map solver possibleMoves
        oneSolution:otherSolutions = allSolutions
     in foldl compareSolutions oneSolution otherSolutions

trySolveWithMove :: [SlidingPuzzle] -> Int -> Int -> Int -> SlidingPuzzleSolution
trySolveWithMove path@(puzzle:parents) givenLoss tries tile =
  let movedPuzzle = moveTile puzzle tile
      solutionSoFar
        | elem movedPuzzle parents = makeLeafSlidingPuzzleSolution givenLoss
        | otherwise =
          let tryLoss = givenLoss + lossForMove puzzle tile
              solution = trySolveGivenLoss (movedPuzzle:path) tryLoss (tries - 1)
           in makeParentSlidingPuzzleSolution solution tile
   in solutionSoFar

data SlidingPuzzleSolution = SlidingPuzzleSolution
  { bestLoss  :: Int
  , bestMoves :: [Int]
  }

makeLeafSlidingPuzzleSolution :: Int -> SlidingPuzzleSolution
makeLeafSlidingPuzzleSolution leafLoss =
  SlidingPuzzleSolution { bestLoss = leafLoss, bestMoves = [] }

makeParentSlidingPuzzleSolution :: SlidingPuzzleSolution -> Int -> SlidingPuzzleSolution
makeParentSlidingPuzzleSolution child tile =
  SlidingPuzzleSolution { bestLoss = bestLoss child, bestMoves = tile : bestMoves child }

compareSolutions ::
     SlidingPuzzleSolution -> SlidingPuzzleSolution -> SlidingPuzzleSolution
compareSolutions first second
  | bestLoss first < bestLoss second = first
  | otherwise = second
