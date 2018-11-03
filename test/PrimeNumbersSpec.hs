module PrimeNumbersSpec where

import PrimeNumbers
import Test.Tasty
import Test.Tasty.HUnit

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
      assertEqual "Multiples are the same" (multiplesOfPrimes generator) (multiplesOfPrimes generator5)

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
