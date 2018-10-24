module MainSpec where

import           Control.Exception
import           MainArgs
import           Test.Tasty
import           Test.Tasty.HUnit

test_parseBowlingGameArgs :: TestTree
test_parseBowlingGameArgs =
  let args = parseBowlingGameArgs ["3", "7", "8"]
   in testCase "Testing parse BowlingGame arguments" $
      assertEqual "Args should be 3 7 8" [3, 7, 8] (rolls args)

test_parseSlidingPuzzleArgs :: TestTree
test_parseSlidingPuzzleArgs =
  let args = parseSlidingPuzzleArgs ["100"]
   in testCase "Testing parse SlidingPuzzle arguments" $
      assertEqual "Args should be 100" 100 (shuffleMoves args)

test_parseSlidingPuzzleTrailingArgs :: TestTree
test_parseSlidingPuzzleTrailingArgs =
  let args = parseSlidingPuzzleArgs ["100", "dummy"]
   in testCase "Testing sliding puzzle trailing args" $ do
        errored <-
          try (evaluate args) :: IO (Either SomeException SlidingPuzzleArgs)
        case errored of
          Right _ -> failedSlidingPuzzleTrailingArgs
          Left _  -> assertSomeException

assertSomeException :: IO ()
assertSomeException = pure ()

failedSlidingPuzzleTrailingArgs :: IO ()
failedSlidingPuzzleTrailingArgs =
  assertFailure "Did not catch trailing args error"
