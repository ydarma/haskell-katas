module Mcmc where

import           Data.List
import           Data.Maybe
import           System.Random

class MarkovChain m where
  randomWalk :: RandomGen g => MarkovState m -> g -> (MarkovState m, g)
  conditionalProbability :: MarkovState m -> MarkovState m -> Double

data MarkovState m = MarkovState
  { state :: m
  , loss  :: Double
  }

accept ::
     (MarkovChain m, RandomGen g)
  => MarkovState m
  -> MarkovState m
  -> Double
  -> g
  -> (Bool, g)
accept current@MarkovState {state = currState, loss = currLoss}
       new@MarkovState { state = newState , loss = newLoss } beta rgen =
  let proba = conditionalProbability new current
      backProba = conditionalProbability current new
      diffLoss = currLoss - newLoss
      rgibbs = exp (diffLoss / beta) * backProba / proba
      (runif, ggen) = randomR (0.0, 1.0) rgen
   in (runif < rgibbs, ggen)

iter ::
     (MarkovChain m, RandomGen g)
  => (MarkovState m, g)
  -> Double
  -> (MarkovState m, g)
iter (current, rgen) beta =
  let (new, ggen) = randomWalk current rgen
      (accepted, ngen) = accept current new beta ggen
   in if accepted
        then (new, ngen)
        else (current, ngen)

mcmc ::
     (MarkovChain m, RandomGen g)
  => (MarkovState m, g)
  -> [Double]
  -> (MarkovState m, g)
mcmc current [] = current
mcmc (current@MarkovState {loss = 0}, rgen) _ = (current, rgen)
mcmc currState (beta:betaSequence) =
  let new = iter currState beta
   in mcmc new betaSequence
