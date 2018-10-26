module McmcSpec where

import           Mcmc
import           McmcSlidingPuzzle
import qualified SlidingPuzzle     as SP
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit

test_accept :: TestTree
test_accept =
  let initState = makePuzzleState SP.makePuzzle
      (newState, ggen) = randomWalk initState (mkStdGen 324)
      (accepted, _) = accept newState initState 0.1 ggen
   in testCase "Testing mcmc proposal" $
      assertBool "Better loss should be accepted" accepted

test_iter :: TestTree
test_iter =
  let (puzzle, _, rgen) = SP.shuffle SP.makePuzzle 3 (mkStdGen 324)
      initState = makePuzzleState puzzle
      (iteredState, ggen) = iter (initState, rgen) 0.1
      finalLoss = fromIntegral $ SP.loss (value iteredState)
   in testCase "Testing mcmc iteration" $
      assertEqual "Loss should be equal" finalLoss (loss iteredState)

test_mcmc :: TestTree
test_mcmc =
  let (puzzle, _, rgen) = SP.shuffle SP.makePuzzle 200 (mkStdGen 324)
      initState = makePuzzleState puzzle
      betaSequence = replicate 1000 1
      (finalState, _) = mcmc (initState, rgen) betaSequence
   in testCase "Testing mcmc run" $
      assertBool "Loss should have decreased " (loss finalState < loss initState)
