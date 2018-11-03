module PrimeNumbers where

data OddPrimeGenerator = OddPrimeGenerator { primes :: [Int], multiplesOfPrimes :: [Int] }

makeOddPrimeGenerator :: OddPrimeGenerator
makeOddPrimeGenerator = OddPrimeGenerator { primes = [3], multiplesOfPrimes = [] }

isSquareOfNextRelevantPrime :: OddPrimeGenerator -> Int -> (OddPrimeGenerator, Bool)
isSquareOfNextRelevantPrime generator candidate =
  let nextRelevant = primes generator !! (length $ multiplesOfPrimes generator)
      nextSquare = nextRelevant * nextRelevant
      isNextSquare = candidate == nextSquare
      newGenerator
        | isNextSquare = addMultiple generator nextSquare
        | otherwise = generator
   in (newGenerator, isNextSquare)

isMultipleOfPrime :: OddPrimeGenerator -> Int -> (OddPrimeGenerator, Bool)
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

addMultiple :: OddPrimeGenerator -> Int -> OddPrimeGenerator
addMultiple generator multiple =
  OddPrimeGenerator {
    primes = primes generator,
    multiplesOfPrimes = multiplesOfPrimes generator ++ [multiple]
  }

addPrime :: OddPrimeGenerator -> Int -> OddPrimeGenerator
addPrime generator prime =
  OddPrimeGenerator {
    primes = primes generator ++ [prime],
    multiplesOfPrimes = multiplesOfPrimes generator
  }
