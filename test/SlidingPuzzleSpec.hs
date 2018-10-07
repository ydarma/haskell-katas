module SlidingPuzzleSpec where

import           SlidingPuzzle
import           Test.Tasty
import           Test.Tasty.HUnit

test_buildPuzzle :: TestTree
test_buildPuzzle =
  let puzzle = buildPuzzle
   in testCase "Testing build SlidingPuzzle" $
      assertEqual "Puzzle should be initialized" [0 .. 15] (tiles puzzle)

test_neighbors :: TestTree
test_neighbors =
  testCase "Testing neightbors" $
  assertEqual "Slot 0 has neighbors 1 and 4" [1, 4] (neighborsOfSlot 0) >>
  assertEqual "Slot 1 has neighbors 0, 2 and 5" [0, 2, 5] (neighborsOfSlot 1) >>
  assertEqual "Slot 2 has neighbors 1, 3 and 6" [1, 3, 6] (neighborsOfSlot 2) >>
  assertEqual "Slot 3 has neighbors 2 and 7" [2, 7] (neighborsOfSlot 3) >>
  assertEqual "Slot 4 has neighbors 0, 5 and 8" [0, 5, 8] (neighborsOfSlot 4) >>
  assertEqual
    "Slot 5 has neighbors 1, 4, 6 and 9"
    [1, 4, 6, 9]
    (neighborsOfSlot 5) >>
  assertEqual
    "Slot 6 has neighbors 2, 5, 7 and 10"
    [2, 5, 7, 10]
    (neighborsOfSlot 6) >>
  assertEqual "Slot 7 has neighbors 3, 6 and 11" [3, 6, 11] (neighborsOfSlot 7) >>
  assertEqual "Slot 8 has neighbors 4, 9 and 12" [4, 9, 12] (neighborsOfSlot 8) >>
  assertEqual
    "Slot 9 has neighbors 5, 8, 10 and 13"
    [5, 8, 10, 13]
    (neighborsOfSlot 9) >>
  assertEqual
    "Slot 10 has neighbors 6, 9, 11 and 14"
    [6, 9, 11, 14]
    (neighborsOfSlot 10) >>
  assertEqual
    "Slot 11 has neighbors 7, 10 and 15"
    [7, 10, 15]
    (neighborsOfSlot 11) >>
  assertEqual "Slot 12 has neighbors 8 and 13" [8, 13] (neighborsOfSlot 12) >>
  assertEqual
    "Slot 13 has neighbors 9, 12 and 14"
    [9, 12, 14]
    (neighborsOfSlot 13) >>
  assertEqual
    "Slot 14 has neighbors 10, 13 and 15"
    [10, 13, 15]
    (neighborsOfSlot 14) >>
  assertEqual "Slot 15 has neighbors 11 and 14" [11, 14] (neighborsOfSlot 15)

test_moveTile :: TestTree
test_moveTile =
  let puzzle = buildPuzzle
      movedPuzzle = moveTile puzzle 4
   in testCase "Testing tile move" $
      assertEqual "Tile 4 should be in slot 0" 0 (tiles movedPuzzle !! 4) >>
      assertEqual "Tile 3 should be in slot 3" 3 (tiles movedPuzzle !! 3) >>
      assertEqual "Tile 5 should be in slot 5" 5 (tiles movedPuzzle !! 5) >>
      assertEqual "Empty slot should be 4" 4 (head $ tiles movedPuzzle) >>
      assertEqual "Number of slots should be 16" 16 (length $ tiles movedPuzzle)
