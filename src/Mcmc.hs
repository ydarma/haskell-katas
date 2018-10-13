module Mcmc where

import           Data.List
import           Data.Maybe
import           System.Random

class MarkovChain m where
  randomWalk :: RandomGen g => m -> g -> (m, Double, g)
  conditionalProbabilities :: m -> m -> (Double, Double)

accept ::
     (MarkovChain m, RandomGen g)
  => m
  -> (m, Double)
  -> Double
  -> g
  -> (Bool, g)
accept chain walk beta rgen =
  let (otherChain, diff) = walk
      (proba, backProba) = conditionalProbabilities chain otherChain
      rgibbs = exp (diff / beta) * backProba / proba
      (runif, ggen) = randomR (0.0, 1.0) rgen
   in (rgibbs < runif, ggen)

iter :: (MarkovChain m, RandomGen g) => m -> Double -> g -> (m, Double, g)
iter chain beta rgen =
  let (walk, diff, ggen) = randomWalk chain rgen
      (accepted, ngen) = accept chain (walk, diff) beta ggen
   in if accepted
        then (walk, diff, ngen)
        else (chain, 0, ngen)

mcmc ::
     (MarkovChain m, RandomGen g)
  => (m, Double)
  -> Int
  -> Double
  -> g
  -> (m, Double, g)
mcmc chain numIter beta rgen =
  let (currChain, currLoss) = chain
      doIter current _ =
        let (currChain, currLoss, ggen) = current
            (nextChain, diff, ngen) = iter currChain beta ggen
         in (nextChain, currLoss + diff, ngen)
      iterations = scanl doIter (currChain, currLoss, rgen) [0 .. numIter]
      stop current =
        let (_, currLoss, _) = current
         in currLoss == 0
      solution = find stop iterations
   in fromMaybe (last iterations) solution
