module PrimeNumbersSpec where

import           PrimeNumbers
import           Test.Tasty
import           Test.Tasty.HUnit

test_build :: TestTree
test_build =
  let generator = makeOddPrimeGenerator
   in testCase "Testing generator" $
      assertEqual "Primes is [3]" [3] (primes generator) >>
      assertEqual "Multiples is []" [] (multiplesOfPrimes generator)

test_addMultiple :: TestTree
test_addMultiple =
  let generator = makeOddPrimeGenerator
      generator9 = addMultiple generator 9
   in testCase "Testing add multiple" $
      assertEqual "Primes are the same" (primes generator) (primes generator9) >>
      assertEqual "Multiples are [9]" [9] (multiplesOfPrimes generator9)

test_addPrime :: TestTree
test_addPrime =
  let generator = makeOddPrimeGenerator
      generator5 = addPrime generator 5
   in testCase "Testing add prime" $
      assertEqual "Primes are [3, 5]" [3, 5] (primes generator5) >>
      assertEqual
        "Multiples are the same"
        (multiplesOfPrimes generator)
        (multiplesOfPrimes generator5)

test_isSquareOfNextRelevantPrime :: TestTree
test_isSquareOfNextRelevantPrime =
  let generator = makeOddPrimeGenerator
      (generator5, isSquare5) = isSquareOfNextRelevantPrime generator 5
      (generator9, isSquare9) = isSquareOfNextRelevantPrime generator 9
   in testCase "Testing square of next relevant prime" $
      assertBool "5 is not square of next relevant prime" (not isSquare5) >>
      assertEqual "Multiples is []" [] (multiplesOfPrimes generator5) >>
      assertBool "9 is square of next relevant prime" isSquare9 >>
      assertEqual "Multiples is [9]" [9] (multiplesOfPrimes generator9)

test_isMultipleOfPrime :: TestTree
test_isMultipleOfPrime =
  let generator = makeOddPrimeGenerator
      (generator5, isMultiple5) = isMultipleOfPrime generator 5
   in testCase "Testing multiple of prime" $
      assertBool "5 is not multiple of prime" (not isMultiple5) >>
      assertEqual "Primes are [3, 5]" [3, 5] (primes generator5)

test_multipleOfPrimesIncrement :: TestTree
test_multipleOfPrimesIncrement =
  let generator = makeOddPrimeGenerator
      generator5 = addPrime generator 5
      generator7 = addPrime generator5 7
      generator9 = addMultiple generator7 9
      (generator11, isMultiple11) = isMultipleOfPrime generator9 11
   in testCase "Testing multiple increment" $
      assertBool "11 is not multiple of prime" (not isMultiple11) >>
      assertEqual "Multiples is [15]" [15] (multiplesOfPrimes generator11)

test_isPrime :: TestTree
test_isPrime =
  let generator = makeOddPrimeGenerator
      (generator5, isPrime5) = isPrime generator 5
      (generator7, isPrime7) = isPrime generator5 7
      (generator9, isPrime9) = isPrime generator7 9
      (generator11, isPrime11) = isPrime generator9 11
      (generator13, isPrime13) = isPrime generator11 13
      (generator15, isPrime15) = isPrime generator13 15
   in testCase "Testing prime test" $
      assertEqual "Primes are [3, 5, 7, 11]" [3, 5, 7, 11] (primes generator11) >>
      assertEqual "Multiples is [15]" [15] (multiplesOfPrimes generator11) >>
      assertEqual "Primes are [3, 5, 7, 11, 13]" [3, 5, 7, 11, 13] (primes generator13) >>
      assertEqual "Multiples is [15]" [15] (multiplesOfPrimes generator13) >>
      assertEqual "Primes are [3, 5, 7, 11, 13]" [3, 5, 7, 11, 13] (primes generator15) >>
      assertEqual "Multiples is [15]" [15] (multiplesOfPrimes generator15)

test_generateUntil :: TestTree
test_generateUntil =
  let generator = generatePrimesUntil 30
   in testCase "Testing prime generation" $
      assertEqual
        "Primes are [3, 5, 7, 11, 13, 17, 19, 23, 29]"
        [3, 5, 7, 11, 13, 17, 19, 23, 29]
        (primes generator) >>
      assertEqual "Multiples are [33, 35]" [33, 35] (multiplesOfPrimes generator)

test_notGreaterThan :: TestTree
test_notGreaterThan =
  let generator157 = generatePrimesUntil 157
      generator162 = generatePrimesUntil 162
   in testCase "Testing 157 is prime" $
      assertEqual "157 is prime" 157 (last $ primes generator157) >>
      assertEqual "157 is 36th odd prime" 36 (length $ primes generator157) >>
      assertEqual "No prime between 158 and 162" 157 (last $ primes generator162)
