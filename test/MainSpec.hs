module MainSpec where

import           MainArgs
import           Test.Tasty
import           Test.Tasty.HUnit

test_parseBowlingGameArgs :: TestTree
test_parseBowlingGameArgs =
  let args = parseBowlingGameArgs (["3", "7", "8"])
   in testCase "Testing parse BowlingGame arguments" $ do
        (assertEqual "Args should be 3 7 8" [3, 7, 8] (rolls args))
