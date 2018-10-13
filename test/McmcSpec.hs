module McmcSpec where

import           Mcmc
import           SlidingPuzzle
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit

instance MarkovChain SlidingPuzzle where
  randomWalk puzzle rgen =
    let (movedPuzzle, tile, ggen) = moveRandom puzzle rgen
        lossDiff = fromIntegral $ lossForMove puzzle tile
     in (movedPuzzle, lossDiff, ggen)
  conditionalProbabilities puzzle otherPuzzle =
    let neighbors = neighborsOfSlot (emptySlot puzzle)
        otherNeighbors = neighborsOfSlot (emptySlot otherPuzzle)
        probability = 1.0 / fromIntegral (length neighbors)
        otherProbability = 1.0 / fromIntegral (length otherNeighbors)
     in (probability, otherProbability)

test_accept :: TestTree
test_accept =
  let puzzle = buildPuzzle
      (walk, diff, ggen) = randomWalk puzzle (mkStdGen 324)
      (accepted, _) = accept walk (puzzle, -diff) 0.1 ggen
   in testCase "Testing mcmc proposal" $
      assertBool "Better loss should be accepted" accepted

test_iter :: TestTree
test_iter =
  let (puzzle, _, rgen) = shuffle buildPuzzle 3 (mkStdGen 324)
      initLoss = fromIntegral $ loss puzzle
      (iteredPuzzle, iteredLoss, ggen) = iter puzzle 0.1 rgen
      finalLoss = fromIntegral $ loss iteredPuzzle
   in testCase "Testing mcmc iteration" $
      assertEqual "Loss should be equal" finalLoss (initLoss + iteredLoss)

test_mcmc :: TestTree
test_mcmc =
  let (puzzle, _, rgen) = shuffle buildPuzzle 10 (mkStdGen 324)
      (mcmcPuzzle, mcmcLoss, _) =
        mcmc (puzzle, fromIntegral $ loss puzzle) 30 0.1 rgen
   in testCase "Testing mcmc run" $
      assertEqual
        "Loss should be equal"
        (fromIntegral $ loss mcmcPuzzle)
        mcmcLoss >>
      assertBool "Loss should have decreased " (loss mcmcPuzzle < loss puzzle)
