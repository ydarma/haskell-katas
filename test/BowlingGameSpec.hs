{-# LANGUAGE ScopedTypeVariables #-}

module BowlingGameSpec where

import           BowlingGame
import           Control.Exception
import           Test.Tasty
import           Test.Tasty.HUnit

test_makeGame :: TestTree
test_makeGame =
  let game = makeGame
   in testCase
        "Testing make BowlingGame" $
        assertEqual "Score should be 0" 0 (score game)

test_play :: TestTree
test_play =
  let game = makeGame
      gameAfertOneRoll = play game 3
   in testCase "Testing play 3" $
      assertEqual "Score should be 3" 3 (score gameAfertOneRoll) >>
      assertEqual "Turn should be 0, 1" (0, 1) (rollOffset gameAfertOneRoll)

test_playTwice :: TestTree
test_playTwice =
  let game = makeGame
      playTwice = play . play game
      gameAfterTwoRolls = playTwice 3 6
   in testCase "Testing play 3 then 6" $
      assertEqual "Score should be 9" 9 (score gameAfterTwoRolls) >>
      assertEqual "Turn should be 1, 0" (1, 0) (rollOffset gameAfterTwoRolls)

test_playMore :: TestTree
test_playMore =
  let game = makeGame
      gameAfterThreeRolls = playAll game [3, 6, 1]
   in testCase "Testing play 3 then 6 the 1" $
      assertEqual "Score should be 10" 10 (score gameAfterThreeRolls) >>
      assertEqual "Turn should be 1, 1" (1, 1) (rollOffset gameAfterThreeRolls)

test_spare :: TestTree
test_spare =
  let game = makeGame
      gameWithSpare = playAll game [1, 2, 8, 1, 3, 7, 4]
   in testCase "Testing play 1, 2, 8, 1, spare, 4" $
      assertEqual "Score should be 30" 30 (score gameWithSpare) >>
      assertEqual "Turn should be 3, 1" (3, 1) (rollOffset gameWithSpare)

test_strike :: TestTree
test_strike =
  let game = makeGame
      gameWithStrike = playAll game [1, 2, 10, 3, 7, 4]
   in testCase "Testing play 1, 2, strike, spare, 4" $
      assertEqual "Score should be 41" 41 (score gameWithStrike) >>
      assertEqual "Turn should be (3, 1)" (3, 1) (rollOffset gameWithStrike)

test_allStrikes :: TestTree
test_allStrikes =
  let game = makeGame
      gameWithAllStrikes = playAll game (replicate 12 10)
   in testCase "Testing play all strikes" $
      assertEqual "Score should be 300" 300 (score gameWithAllStrikes) >>
      assertEqual "Turn should be (12, 0)" (12, 0) (rollOffset gameWithAllStrikes)

test_allSpares :: TestTree
test_allSpares =
  let game = makeGame
      gameWithAllStrikes = playAll game (replicate 21 5)
   in testCase "Testing play all spares" $
      assertEqual "Score should be 150" 150 (score gameWithAllStrikes) >>
      assertEqual "Turn should be (10, 1)" (10, 1) (rollOffset gameWithAllStrikes)

test_ended :: TestTree
test_ended =
  let game = makeGame
      gameEnded = playAll game (replicate 21 4)
   in testCase "Testing game ended" $ do
      errored <-
        try (evaluate (score gameEnded)) :: IO (Either SomeException Int)
      case errored of
        Right _ -> failedInvalidRollError
        Left _  -> assertSomeException

test_endedAfterSpare :: TestTree
test_endedAfterSpare =
  let game = makeGame
      gameEnded = playAll game (replicate 22 5)
   in testCase "Testing game ended after spare" $ do
      errored <-
        try (evaluate (score gameEnded)) :: IO (Either SomeException Int)
      case errored of
        Right _ -> failedInvalidRollError
        Left _  -> assertSomeException

test_endedAfterStrike :: TestTree
test_endedAfterStrike =
  let game = makeGame
      gameEnded = playAll game (replicate 13 10)
   in testCase "Testing game ended after strike" $ do
      errored <-
        try (evaluate (score gameEnded)) :: IO (Either SomeException Int)
      case errored of
        Right _ -> failedInvalidRollError
        Left _  -> assertSomeException

test_invalidFirstRoll :: TestTree
test_invalidFirstRoll =
  let game = makeGame
      gameWithInvalidRoll = playAll game [11, 3]
   in testCase "Testing invalid first roll" $ do
      errored <-
        try (evaluate (score gameWithInvalidRoll)) :: IO (Either SomeException Int)
      case errored of
        Right _ -> failedInvalidRollError
        Left _  -> assertSomeException

test_invalidSecondRoll :: TestTree
test_invalidSecondRoll =
  let game = makeGame
      gameWithInvalidRoll = playAll game [4, 7, 3]
   in testCase "Testing game ended after strike" $ do
      errored <-
        try (evaluate (score gameWithInvalidRoll)) :: IO (Either SomeException Int)
      case errored of
        Right _ -> failedInvalidRollError
        Left _  -> assertSomeException


assertSomeException :: IO ()
assertSomeException = pure ()

failedGameEndedError :: IO ()
failedGameEndedError = assertFailure "Did not catch game ended error"

failedInvalidRollError :: IO ()
failedInvalidRollError = assertFailure "Did not catch invalid roll error"
