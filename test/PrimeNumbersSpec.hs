module PrimeNumbersSpec where

import PrimeNumbers
import Test.Tasty
import Test.Tasty.HUnit

test_build :: TestTree
test_build =
  let generator = makePrimeGenerator
   in testCase "Testing generator" $
      assertEqual "Primes is [2]" [2] (primes generator) >>
      assertEqual "Multiples is []" [] (multiplesOfPrimes generator)

test_addMultiple :: TestTree
test_addMultiple =
  let generator = makePrimeGenerator
      generator4 = addMultiple generator 4
   in testCase "Testing add multiple" $
      assertEqual "Primes are the same" (primes generator) (primes generator4) >>
      assertEqual "Multiples are [4]" [4] (multiplesOfPrimes generator4)

test_addPrime :: TestTree
test_addPrime =
  let generator = makePrimeGenerator
      generator3 = addPrime generator 3
   in testCase "Testing add prime" $
      assertEqual "Primes are [2, 3]" [2, 3] (primes generator3) >>
      assertEqual "Multiples are the same" (multiplesOfPrimes generator) (multiplesOfPrimes generator3)

test_isSquareOfNextRelevantPrime :: TestTree
test_isSquareOfNextRelevantPrime =
  let generator = makePrimeGenerator
      (generator3, isSquare3) = isSquareOfNextRelevantPrime generator 3
      (generator4, isSquare4) = isSquareOfNextRelevantPrime generator 4
   in testCase "Testing square of next relevant prime" $
      assertBool "3 is not square of next relevant prime" (not isSquare3) >>
      assertEqual "Multiples is []" [] (multiplesOfPrimes generator3) >>
      assertBool "4 is square of next relevant prime" isSquare4 >>
      assertEqual "Multiples is [4]" [4] (multiplesOfPrimes generator4)

test_isMultipleOfPrime :: TestTree
test_isMultipleOfPrime =
  let generator = makePrimeGenerator
      (generator3, isMultiple3) = isMultipleOfPrime generator 3
   in testCase "Testing multiple of prime" $
      assertBool "3 is not multiple of prime" (not isMultiple3) >>
      assertEqual "Primes are [2, 3]" [2, 3] (primes generator3)
