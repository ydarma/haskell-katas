module McmcSlidingPuzzle
  ( makePuzzleState
  ) where

import           Mcmc
import qualified SlidingPuzzle as SP
import           System.Random

-- enable mcmc optimization to solve a SlidingPuzzle
instance MarkovChain SP.SlidingPuzzle where
  randomWalk = spRandomWalk
  conditionalProbability = spConditionalProbability

-- move a tile randomly from the given puzzle and compute the new loss
spRandomWalk ::
     RandomGen g
  => MarkovState SP.SlidingPuzzle
  -> g
  -> (MarkovState SP.SlidingPuzzle, g)
spRandomWalk MarkovState {value = initPuzzle, loss = initLoss} rgen =
  let (movedPuzzle, tile, ggen) = SP.moveRandom initPuzzle rgen
      diffLoss = fromIntegral $ SP.lossForMove initPuzzle tile
      new = MarkovState {value = movedPuzzle, loss = initLoss + diffLoss}
   in (new, ggen)

-- compute the probabilities for moving from on puzzle to the other
spConditionalProbability ::
     MarkovState SP.SlidingPuzzle -> MarkovState SP.SlidingPuzzle -> Double
spConditionalProbability MarkovState {value = puzzle} MarkovState {value = givenPuzzle} =
  let neighbors = SP.neighborsOfSlot (SP.emptySlot puzzle)
      probability = 1.0 / fromIntegral (length neighbors)
   in probability

-- build a MarkovState from a SlidingPuzzle
makePuzzleState :: SP.SlidingPuzzle -> MarkovState SP.SlidingPuzzle
makePuzzleState puzzle =
  MarkovState {value = puzzle, loss = fromIntegral $ SP.loss puzzle}
