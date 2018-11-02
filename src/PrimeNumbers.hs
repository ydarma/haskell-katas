module PrimeNumbers where

data PrimeGenerator = PrimeGenerator { primes :: [Int], multiplesOfPrimes :: [Int] }

makePrimeGenerator :: PrimeGenerator
makePrimeGenerator = PrimeGenerator { primes = [2], multiplesOfPrimes = [] }

isSquareOfNextRelevantPrime :: PrimeGenerator -> Int -> (PrimeGenerator, Bool)
isSquareOfNextRelevantPrime generator candidate =
  let nextRelevant = primes generator !! (length $ multiplesOfPrimes generator)
      nextSquare = nextRelevant * nextRelevant
      isNextSquare = candidate == nextSquare
      newGenerator
        | isNextSquare = addMultiple generator nextSquare
        | otherwise = generator
   in (newGenerator, isNextSquare)

isMultipleOfPrime :: PrimeGenerator -> Int -> (PrimeGenerator, Bool)
isMultipleOfPrime generator candidate =
  let incr multiple = if multiple < candidate
                        then multiple
                        else incr (multiple + 2 * candidate)
      check [] = False
      check (base:others) =
         let multiple = incr base
         in if multiple == candidate
           then True
           else check others
      isMultiple = check (primes generator)
      newGenerator
        | isMultiple = generator
        | otherwise = addPrime generator candidate
   in (newGenerator, isMultiple)

addMultiple :: PrimeGenerator -> Int -> PrimeGenerator
addMultiple generator multiple =
  PrimeGenerator {
    primes = primes generator,
    multiplesOfPrimes = multiplesOfPrimes generator ++ [multiple]
  }

addPrime :: PrimeGenerator -> Int -> PrimeGenerator
addPrime generator prime =
  PrimeGenerator {
    primes = primes generator ++ [prime],
    multiplesOfPrimes = multiplesOfPrimes generator
  }
