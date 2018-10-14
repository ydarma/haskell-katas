module McmcSpec where

import           Mcmc
import           McmcSlidingPuzzle
import qualified SlidingPuzzle     as SP
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit

test_accept :: TestTree
test_accept =
  let puzzle = SP.buildPuzzle
      initState =
        MarkovState {state = puzzle, loss = fromIntegral $ SP.loss puzzle}
      (newState, ggen) = randomWalk initState (mkStdGen 324)
      (accepted, _) = accept newState initState 0.1 ggen
   in testCase "Testing mcmc proposal" $
      assertBool "Better loss should be accepted" accepted

test_iter :: TestTree
test_iter =
  let (puzzle, _, rgen) = SP.shuffle SP.buildPuzzle 3 (mkStdGen 324)
      initLoss = fromIntegral $ SP.loss puzzle
      initState = MarkovState {state = puzzle, loss = initLoss}
      (iteredState, ggen) = iter (initState, rgen) 0.1
      finalLoss = fromIntegral $ SP.loss (state iteredState)
   in testCase "Testing mcmc iteration" $
      assertEqual "Loss should be equal" finalLoss (loss iteredState)

test_mcmc :: TestTree
test_mcmc =
  let (puzzle, _, rgen) = SP.shuffle SP.buildPuzzle 200 (mkStdGen 324)
      initLoss = fromIntegral $ SP.loss puzzle
      initState = MarkovState {state = puzzle, loss = initLoss}
      betaSequence = replicate 1000 1
      (mcmcState, _) = mcmc (initState, rgen) betaSequence
      finalLoss = fromIntegral $ SP.loss (state mcmcState)
   in testCase "Testing mcmc run" $
      assertEqual "Loss should be equal" finalLoss (loss mcmcState) >>
      assertBool "Loss should have decreased " (finalLoss < initLoss)
