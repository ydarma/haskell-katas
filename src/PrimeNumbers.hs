module PrimeNumbers where

import Data.Sequence

data PrimeGenerator = PrimeGenerator { primes :: Seq Int, multiplesOfPrimes :: Seq Int }

makePrimeGenerator :: PrimeGenerator
makePrimeGenerator = PrimeGenerator { primes = singleton 2, multiplesOfPrimes = empty }

updateIfGreaterThanLastPrimeSquare :: PrimeGenerator -> Int -> (PrimeGenerator, Bool)
updateIfGreaterThanLastPrimeSquare generator n =
  let (_:|>lastPrime) = primes generator
      lastPrimeSquare = lastPrime * lastPrime
      update = lastPrimeSquare <= n
      returnGenerator
        | update = addMultiple generator lastPrimeSquare
        | otherwise = generator
   in (returnGenerator, update)

addMultiple :: PrimeGenerator -> Int -> PrimeGenerator
addMultiple generator multiple =
  PrimeGenerator {
    primes = primes generator,
    multiplesOfPrimes = (multiplesOfPrimes generator) :|> multiple
  }

