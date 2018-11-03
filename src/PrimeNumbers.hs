module PrimeNumbers where

data OddPrimeGenerator = OddPrimeGenerator
  { primes            :: [Int]
  , multiplesOfPrimes :: [Int]
  }

makeOddPrimeGenerator :: OddPrimeGenerator
makeOddPrimeGenerator = OddPrimeGenerator {primes = [3], multiplesOfPrimes = []}

generatePrimesUntil :: Int -> OddPrimeGenerator
generatePrimesUntil upper =
  let accumulate generator candidate = fst (isPrime generator candidate)
   in foldl accumulate makeOddPrimeGenerator [5, 7..upper]

isPrime :: OddPrimeGenerator -> Int -> (OddPrimeGenerator, Bool)
isPrime generator candidate =
  let (squareGenerator, isSquare) =
        isSquareOfNextRelevantPrime generator candidate
      (multipleGenerator, isMultiple) =
        isMultipleOfPrime squareGenerator candidate
   in if isSquare
        then (squareGenerator, False)
        else (multipleGenerator, not isMultiple)

isSquareOfNextRelevantPrime ::
     OddPrimeGenerator -> Int -> (OddPrimeGenerator, Bool)
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
  let multiplesAndPrimes = zip (multiplesOfPrimes generator) (primes generator)
      (isMultiple, multiples) =
        updateMultiplesForCandidate candidate multiplesAndPrimes
      generatorIfPrime = addPrime generator candidate
      generatorUpdate
        | isMultiple = setMultiples generator multiples
        | otherwise = setMultiples generatorIfPrime multiples
   in (generatorUpdate, isMultiple)

updateMultiplesForCandidate :: Int -> [(Int, Int)] -> (Bool, [Int])
updateMultiplesForCandidate candidate multiplesAndPrimes =
  let incr (multiple, prime) =
        if multiple < candidate
          then incr ((multiple + 2 * prime), prime)
          else multiple
      update [] = (False, [])
      update (next:others) =
        let multiple = incr next
            (isOtherMultiple, otherMultiples) = update others
         in if multiple == candidate
              then (True, multiple : map fst others)
              else (isOtherMultiple, multiple : otherMultiples)
   in update multiplesAndPrimes

addMultiple :: OddPrimeGenerator -> Int -> OddPrimeGenerator
addMultiple generator multiple =
  OddPrimeGenerator
    { primes = primes generator
    , multiplesOfPrimes = multiplesOfPrimes generator ++ [multiple]
    }

addPrime :: OddPrimeGenerator -> Int -> OddPrimeGenerator
addPrime generator prime =
  OddPrimeGenerator
    { primes = primes generator ++ [prime]
    , multiplesOfPrimes = multiplesOfPrimes generator
    }

setMultiples :: OddPrimeGenerator -> [Int] -> OddPrimeGenerator
setMultiples generator multiples =
  OddPrimeGenerator {primes = primes generator, multiplesOfPrimes = multiples}
