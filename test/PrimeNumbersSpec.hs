module PrimeNumbersSpec where

import PrimeNumbers
import Data.Sequence
import Test.Tasty
import Test.Tasty.HUnit

test_build :: TestTree
test_build =
  let generator = makePrimeGenerator
   in testCase "Testing generator" $
      assertEqual "Generator contains 2" (singleton 2) (primes generator)

test_updateIfGreaterThanLastPrimeSquare :: TestTree
test_updateIfGreaterThanLastPrimeSquare =
  let generator = makePrimeGenerator
      (generator3, upd3) = updateIfGreaterThanLastPrimeSquare generator 3
      (generator4, upd4) = updateIfGreaterThanLastPrimeSquare generator 4
   in testCase "Testing greater than last prime square" $
      assertBool "Generator not updated" (not upd3) >>
      assertEqual "Multiples are empty" Empty (multiplesOfPrimes generator3) >>
      assertBool "Generator updated" upd4 >>
      assertEqual "Multiples is 4" (singleton 4) (multiplesOfPrimes generator4)
   
