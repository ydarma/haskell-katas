module McmcSlidingPuzzle where

import           Mcmc
import qualified SlidingPuzzle as SP

instance MarkovChain SP.SlidingPuzzle where
  randomWalk MarkovState {state = initPuzzle, loss = initLoss} rgen =
    let (movedPuzzle, tile, ggen) = SP.moveRandom initPuzzle rgen
        diffLoss = fromIntegral $ SP.lossForMove initPuzzle tile
        new = MarkovState {state = movedPuzzle, loss = initLoss + diffLoss}
     in (new, ggen)
  conditionalProbability MarkovState {state = puzzle} MarkovState {state = givenPuzzle} =
    let neighbors = SP.neighborsOfSlot (SP.emptySlot puzzle)
        probability = 1.0 / fromIntegral (length neighbors)
     in probability

makePuzzleState :: SP.SlidingPuzzle -> MarkovState SP.SlidingPuzzle
makePuzzleState puzzle = MarkovState { state = puzzle, loss = fromIntegral $ SP.loss puzzle }
