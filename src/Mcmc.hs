module Mcmc where

import           Data.List
import           Data.Maybe
import           System.Random

-- instances of a markov chain can be optimized with a MH algorithm
class MarkovChain m where
  randomWalk :: RandomGen g => MarkovState m -> g -> (MarkovState m, g)
  conditionalProbability :: MarkovState m -> MarkovState m -> Double

-- a state of a Markov chain with an attached loss to be minimized
data MarkovState m = MarkovState
  { value :: m
  , loss  :: Double
  }

-- new state acceptation in MH algorithm given the temperature ß
accept ::
     (MarkovChain m, RandomGen g)
  => MarkovState m
  -> MarkovState m
  -> Double
  -> g
  -> (Bool, g)
accept currentState@MarkovState {loss = currLoss} newState@MarkovState {loss = newLoss} beta rgen =
  let proba = conditionalProbability newState currentState
      backProba = conditionalProbability currentState newState
      diffLoss = currLoss - newLoss
      rgibbs = exp (diffLoss / beta) * backProba / proba
      (runif, ggen) = randomR (0.0, 1.0) rgen
   in (rgibbs > runif, ggen)

-- an iteration of MH algorithm : walk then accept or reject new state
iter ::
     (MarkovChain m, RandomGen g)
  => (MarkovState m, g)
  -> Double
  -> (MarkovState m, g)
iter (currentState, rgen) beta =
  let (newState, ggen) = randomWalk currentState rgen
      (accepted, ngen) = accept currentState newState beta ggen
   in if accepted
        then (newState, ngen)
        else (currentState, ngen)

-- MH algorithm with an iteration for each given temperature ß
mcmc ::
     (MarkovChain m, RandomGen g)
  => (MarkovState m, g)
  -> [Double]
  -> (MarkovState m, g)
mcmc currentState [] = currentState
mcmc (currentState@MarkovState {loss = 0}, rgen) _ = (currentState, rgen)
mcmc currentState (beta:otherBetas) =
  let newState = iter currentState beta
   in mcmc newState otherBetas
