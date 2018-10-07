module SlidingPuzzle
  ( buildPuzzle
  , tiles
  , neighborsOfSlot
  , moveTile
  , SlidingPuzzle
  ) where

data SlidingPuzzle = SlidingPuzzle
  { tiles :: [Int]
  }

buildPuzzle :: SlidingPuzzle
buildPuzzle = SlidingPuzzle {tiles = [0 .. 15]}

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

moveTile :: SlidingPuzzle -> Int -> SlidingPuzzle
moveTile puzzle tile =
  let (emptySlot:tilePositions) = tiles puzzle
      slot = tilePositions !! (tile - 1)
      newPositions =
        slot :
        take (tile - 1) tilePositions ++ emptySlot : drop tile tilePositions
   in SlidingPuzzle {tiles = newPositions}
