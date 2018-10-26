module SlidingPuzzle where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Maybe
import           System.Random

-- a tile of the puzzle, from 1 to 15
type Tile = Int

-- a slot of the puzzle, form 0 to 15
type Slot = Int

-- a puzzle is described by slot nubmers for each tile and for empty slot
data SlidingPuzzle = SlidingPuzzle
  { emptySlot :: Slot
  , tileSlots :: [Slot]
  } deriving (Eq)

-- puzzle constructor in completed configuration
makePuzzle :: SlidingPuzzle
makePuzzle = SlidingPuzzle {emptySlot = 0, tileSlots = [1 .. 15]}

-- find the slot of the given tile
findTile :: SlidingPuzzle -> Tile -> Slot
findTile puzzle tile
  | tile > 0 && tile < 16 = tileSlots puzzle !! (tile - 1)
  | otherwise = error "invalid tile"

-- return the tile in the given slot
inSlot :: SlidingPuzzle -> Slot -> Tile
inSlot puzzle slot =
  let tiles = tileSlots puzzle
      found = elemIndex slot tiles
   in 1 + fromJust found

-- return the neighbors of the given slot
neighborsOfSlot :: Slot -> [Slot]
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

-- check if two slots are neighbors
isNeighborOfSlot :: Slot -> Slot -> Bool
isNeighborOfSlot slot otherSlot =
  let neighbors = neighborsOfSlot slot
   in elem otherSlot neighbors

-- move all the given tiles subsequently in the empty slot
moveTiles :: SlidingPuzzle -> [Tile] -> SlidingPuzzle
moveTiles = foldl moveTile

-- move the given tile in the empty slot
moveTile :: SlidingPuzzle -> Tile -> SlidingPuzzle
moveTile puzzle tile =
  let empty = emptySlot puzzle
      slot = findTile puzzle tile
      newSlots
        | isNeighborOfSlot slot empty =
          let currentSlots = tileSlots puzzle
           in changeTileSlot currentSlots tile empty
        | otherwise = error "invalid move"
   in SlidingPuzzle {emptySlot = slot, tileSlots = newSlots}

-- change the slot of a tile in the given tile configuration
changeTileSlot :: [Tile] -> Tile -> Slot -> [Tile]
changeTileSlot tiles tile slot =
  let (left, _:right) = splitAt (tile - 1) tiles
   in left ++ slot : right

-- compute the distance of the given puzzle from the completed configuration
loss :: SlidingPuzzle -> Int
loss puzzle =
  let lossesForAll = map (lossForTile puzzle) [1 .. 15]
   in sum lossesForAll

-- compute the distance of a tile from its final position
lossForTile :: SlidingPuzzle -> Tile -> Int
lossForTile puzzle tile =
  let slot = findTile puzzle tile
   in dist tile slot

-- compute the distance gain if moving the given tile in the empty slot
lossForMove :: SlidingPuzzle -> Tile -> Int
lossForMove puzzle tile =
  let slot = findTile puzzle tile
      empty = emptySlot puzzle
   in dist tile empty - dist tile slot

-- compute the distance of a tile from a slot
dist :: Tile -> Slot -> Int
dist tile slot =
  let tileRow = div tile 4
      slotRow = div slot 4
      tileCol = mod tile 4
      slotCol = mod slot 4
   in abs (tileRow - slotRow) + abs (tileCol - slotCol)

-- move a random chosen tile in the given puzzle
moveRandom :: RandomGen g => SlidingPuzzle -> g -> (SlidingPuzzle, Tile, g)
moveRandom puzzle rgen =
  let possibleSlots = neighborsOfSlot (emptySlot puzzle)
      (idx, ngen) = randomR (0, length possibleSlots - 1) rgen
      chosenSlot = possibleSlots !! idx
      chosenTile = inSlot puzzle chosenSlot
      movedPuzzle = moveTile puzzle chosenTile
   in (movedPuzzle, chosenTile, ngen)

-- shuffle a puzzle with the given number of random moves
shuffle ::
     RandomGen g => SlidingPuzzle -> Int -> g -> (SlidingPuzzle, [Tile], g)
shuffle puzzle 0 rgen = (puzzle, [], rgen)
shuffle puzzle moves rgen =
  let (movedPuzzle, tile, ngen) = moveRandom puzzle rgen
      (shuffledPuzzle, nextMoves, ggen) = shuffle movedPuzzle (moves - 1) ngen
   in (shuffledPuzzle, tile : nextMoves, ggen)

-- solve the given puzzle with a brute force algorithm
bruteSolve :: SlidingPuzzle -> SlidingPuzzleSolution
bruteSolve puzzle = ida puzzle 1

-- iterative deepening A* algorithm
ida :: SlidingPuzzle -> Int -> SlidingPuzzleSolution
ida puzzle tries =
  let solution = trySolve puzzle tries
      minimalTries = length (moves solution) + finalLoss solution
      deeperTries
        | tries < minimalTries = minimalTries
        | otherwise = tries + 1
      finalSolution
        | finalLoss solution == 0 = solution
        | otherwise = ida puzzle deeperTries
   in finalSolution

-- try solve a puzzle in the given number of tries
trySolve :: SlidingPuzzle -> Int -> SlidingPuzzleSolution
trySolve puzzle totalTries =
  let initSolution = makeSolutionFromPuzzle puzzle
      finalSolutions = iterareSolution initSolution (totalTries, 0)
   in fromJust finalSolutions

-- iterate from an incomplete solution to a deeper valid solution, if exists
iterareSolution ::
     SlidingPuzzleSolution -> (Int, Int) -> Maybe SlidingPuzzleSolution
iterareSolution solution tries =
  let (puzzle:_) = path solution
      possibleSlots = neighborsOfSlot (emptySlot puzzle)
      possibleMoves = map (inSlot puzzle) possibleSlots
      allSolutions = mapSolution solution tries possibleMoves
      validSolutions = map fromJust $ filter isJust allSolutions
   in case validSolutions of
        []            -> Nothing
        foudSolutions -> Just $ foldSolutions foudSolutions

-- map a solution to candidate deeper solutions, valid or not
mapSolution ::
     SlidingPuzzleSolution
  -> (Int, Int)
  -> [Int]
  -> [Maybe SlidingPuzzleSolution]
mapSolution solution tries@(totalTries, triesSoFar)
  | totalTries > 7 && triesSoFar < 1 = parMap parSolve solve
  | totalTries > 15 && triesSoFar < 3 = parMap parSolve solve
  | totalTries > 40 && triesSoFar < 6 = parMap parSolve solve
  | otherwise = map solve
  where
    solve = trySolveWithMove solution tries
    parSolve m@(Just solution) = seq (finalLoss solution) return m
    parSolve Nothing           = return Nothing

-- try to build a deeper solution with the given move, valid if unseen before
trySolveWithMove ::
     SlidingPuzzleSolution -> (Int, Int) -> Tile -> Maybe SlidingPuzzleSolution
trySolveWithMove solution (totalTries, triesSoFar) tile
  | elem tryPuzzle parents = Nothing
  | totalTries - triesSoFar <= tryLoss = Just trySolution
  | tryLoss == 0 = Just trySolution
  | otherwise = iterareSolution trySolution (totalTries, triesSoFar + 1)
  where
    trySolution = makeSolutionWithMove solution tile
    (tryPuzzle:_:parents) = path trySolution
    tryLoss = finalLoss trySolution

-- a solution described by loss, moves and all seen configurations
data SlidingPuzzleSolution = SlidingPuzzleSolution
  { finalLoss :: Int
  , moves     :: [Int]
  , path      :: [SlidingPuzzle]
  }

-- compare two solutions, return the best loss
compareSolutions ::
     SlidingPuzzleSolution -> SlidingPuzzleSolution -> SlidingPuzzleSolution
compareSolutions first second
  | finalLoss first < finalLoss second = first
  | otherwise = second

-- build a solution from the given puzzle with no move
makeSolutionFromPuzzle :: SlidingPuzzle -> SlidingPuzzleSolution
makeSolutionFromPuzzle puzzle =
  SlidingPuzzleSolution {finalLoss = loss puzzle, moves = [], path = [puzzle]}

-- build a solution by moving a tile in the given solution
makeSolutionWithMove :: SlidingPuzzleSolution -> Tile -> SlidingPuzzleSolution
makeSolutionWithMove solution tile =
  let givenPath@(puzzle:parents) = path solution
      movedPuzzle = moveTile puzzle tile
      tryLoss = (finalLoss solution) + lossForMove puzzle tile
      tryMoves = (moves solution) ++ [tile]
      tryPath = movedPuzzle : givenPath
   in SlidingPuzzleSolution
        {finalLoss = tryLoss, moves = tryMoves, path = tryPath}

-- find the best solution
foldSolutions :: [SlidingPuzzleSolution] -> SlidingPuzzleSolution
foldSolutions [] = error "cannot fold empty solution list"
foldSolutions [theSolution] = theSolution
foldSolutions (theSolution@SlidingPuzzleSolution {finalLoss = 0}:otherSolutions) =
  theSolution
foldSolutions (oneSolution:otherSolutions) =
  compareSolutions oneSolution $ foldSolutions otherSolutions
