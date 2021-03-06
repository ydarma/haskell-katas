module SlidingPuzzleSpec where

import           Control.Exception
import           SlidingPuzzle
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit

test_makePuzzle :: TestTree
test_makePuzzle =
  let puzzle = makePuzzle
   in testCase "Testing make SlidingPuzzle" $
      assertEqual
        "Tile slots should be initialized"
        [1 .. 15]
        (tileSlots puzzle) >>
      assertEqual "Empty slot should be initialized" 0 (emptySlot puzzle)

test_tileSlot :: TestTree
test_tileSlot =
  let puzzle = makePuzzle
   in testCase "Testing get tile slot" $
      assertEqual "Tile 4 is in slot 4" 4 (findTile puzzle 4) >> do
        errored <-
          try (evaluate (findTile puzzle 17)) :: IO (Either SomeException Int)
        case errored of
          Right _ -> failedInvalidTileError
          Left _  -> assertSomeException

test_neighborOfSlot :: TestTree
test_neighborOfSlot =
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

test_isNeighborOfSlot :: TestTree
test_isNeighborOfSlot =
  testCase "Testing isNeighborOfSlot" $
  assertBool "Slot 4 should be neighbor of slot 0" (isNeighborOfSlot 4 0) >>
  assertBool "Slot 0 should be neighbor of slot 4" (isNeighborOfSlot 0 4) >>
  assertBool
    "Slot 7 should not be neighbor of slot 0"
    (not (isNeighborOfSlot 7 0)) >>
  assertBool
    "Slot 0 should not be neighbor of slot 7"
    (not (isNeighborOfSlot 0 7))

test_moveTile :: TestTree
test_moveTile =
  let puzzle = makePuzzle
      movedPuzzle = moveTile puzzle 4
   in testCase "Testing tile move" $
      assertEqual "Tile 4 should be in slot 0" 0 (findTile movedPuzzle 4) >>
      assertEqual "Tile 3 should be in slot 3" 3 (findTile movedPuzzle 3) >>
      assertEqual "Tile 5 should be in slot 5" 5 (findTile movedPuzzle 5) >>
      assertEqual "Empty slot should be 4" 4 (emptySlot movedPuzzle) >>
      assertEqual
        "Number of tile slots should be 15"
        15
        (length $ tileSlots movedPuzzle)

test_moveTileInvalid :: TestTree
test_moveTileInvalid =
  let puzzle = makePuzzle
      movedPuzzle = moveTile puzzle 7
   in testCase "Testing invalid tile move" $ do
        errored <-
          try (evaluate (findTile movedPuzzle 7)) :: IO (Either SomeException Int)
        case errored of
          Right _ -> failedInvalidMoveError
          Left _  -> assertSomeException

test_moveTiles :: TestTree
test_moveTiles =
  let puzzle = makePuzzle
      movedPuzzle = moveTiles puzzle [4, 5, 1, 4]
   in testCase "Testing multiple moves" $
      assertEqual "Tile 4 should be in slot 1" 1 (findTile movedPuzzle 4) >>
      assertEqual "Tile 5 should be in slot 4" 4 (findTile movedPuzzle 5) >>
      assertEqual "Tile 1 should be in slot 5" 5 (findTile movedPuzzle 1)

test_lossForTile :: TestTree
test_lossForTile =
  let puzzle = makePuzzle
      movedPuzzle = moveTiles puzzle [4, 5, 1, 4]
   in testCase "Testing tile loss" $
      assertEqual "Tile 4 loss should be 2" 2 (lossForTile movedPuzzle 4) >>
      assertEqual "Tile 5 loss should be 1" 1 (lossForTile movedPuzzle 5) >>
      assertEqual "Tile 1 loss should be 1" 1 (lossForTile movedPuzzle 1)

test_loss :: TestTree
test_loss =
  let puzzle = makePuzzle
      movedPuzzle = moveTiles puzzle [4, 5, 1, 4]
   in testCase "Testing tile loss" $
      assertEqual "Puzzle loss should be 4" 4 (loss movedPuzzle)

test_lossForMove :: TestTree
test_lossForMove =
  let puzzle = makePuzzle
   in testCase "Testing differential loss" $
      assertEqual
        "Differential loss for tile 1 should be 1"
        1
        (lossForMove puzzle 1)

test_moveRandom :: TestTree
test_moveRandom =
  let puzzle = makePuzzle
      (movedPuzzle, tile, _) = moveRandom puzzle (mkStdGen 324)
      prevSlot = findTile puzzle tile
   in testCase "Testing random move" $
      assertBool
        "Random tile should has been neighbor of empty Slot"
        (isNeighborOfSlot 0 prevSlot) >>
      assertEqual
        "Previous slot should have becomen empty slot"
        prevSlot
        (emptySlot movedPuzzle) >>
      assertEqual "Puzzle loss should be 1" 1 (loss movedPuzzle)

test_shuffle :: TestTree
test_shuffle =
  let puzzle = makePuzzle
      (shuffledPuzzle, _, _) = shuffle puzzle 200 (mkStdGen 324)
   in testCase "Testing shuffle" $
      assertBool "Loss should be greater than 15" (15 < loss shuffledPuzzle)

test_solveSimple :: TestTree
test_solveSimple =
  let puzzle = makePuzzle
      movedPuzzle = moveTiles puzzle [4, 5, 1, 4]
      solution = trySolve movedPuzzle 4
   in testCase "Testing puzzle solver in fixed tries" $
      assertEqual
        "Solution shoud be 4, 1, 5, 4"
        [4, 1, 5, 4]
        (moves solution) >>
      assertEqual "Loss should be 0" 0 (finalLoss solution)

test_solveBrute :: TestTree
test_solveBrute =
  let puzzle = makePuzzle
      (shuffledPuzzle, _, _) = shuffle puzzle 60 (mkStdGen 324)
      solution = bruteSolve shuffledPuzzle
      playedPuzzle = moveTiles shuffledPuzzle (moves solution)
   in testCase "Testing puzzle brute force solver" $
      assertEqual "Solution loss should be 0" 0 (finalLoss solution) >>
      assertEqual "Final puzzle loss should be 0" 0 (loss playedPuzzle)

assertSomeException :: IO ()
assertSomeException = pure ()

failedInvalidTileError :: IO ()
failedInvalidTileError = assertFailure "Did not catch invalid tile error"

failedInvalidMoveError :: IO ()
failedInvalidMoveError = assertFailure "Did not catch invalid move error"
