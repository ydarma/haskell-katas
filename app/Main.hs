module Main where

import           BowlingGame
import           MainArgs
import           SlidingPuzzle
import           PrimeNumbers
import           Data.List
import           System.Environment
import           System.Random
import           Text.Printf

main :: IO ()
main = fmap run getArgs >>= putStrLn

-- run the kata given by the command line arguments
run :: [String] -> String
run ("BowlingGame":args)   = runBowlingGame (parseBowlingGameArgs args)
run ("SlidingPuzzle":args) = runSlidingPuzzle (parseSlidingPuzzleArgs args)
run ("PrimeNumbers":args)  = runPrimeNumbers (parsePrimeNumbersArgs args)
run _                      = "Kata name required : BowlingGame | SlidingPuzzle"

-- run the bowling game
runBowlingGame :: BowlingGameArgs -> String
runBowlingGame args =
  let playedRolls = rolls args
      playedGame = playAll makeGame playedRolls
   in printf "score=%d" (score playedGame)

-- run the sliding puzzle
runSlidingPuzzle :: SlidingPuzzleArgs -> String
runSlidingPuzzle args =
  let tileMoves = shuffleMoves args
      (shuffledPuzzle, _, _) = shuffle makePuzzle tileMoves (mkStdGen 324)
      solution = bruteSolve shuffledPuzzle
   in printf
        "puzzle=\n%s\nmoves=%s"
        (printSlidingPuzzle shuffledPuzzle)
        (show $ moves solution)

-- run the prime numbers generator
runPrimeNumbers :: PrimeNumbersArgs -> String
runPrimeNumbers args =
  show $ generatePrimesUntil (upper args)

-- display the given puzzle
printSlidingPuzzle :: SlidingPuzzle -> String
printSlidingPuzzle puzzle =
  let slot2Tiles = zip ((emptySlot puzzle):(tileSlots puzzle)) [0..]
      compareSlots = \(s1, t1) -> \(s2, t2) -> compare s1 s2
      slotOrdered = sortBy compareSlots slot2Tiles
      tiles = map (\(s, t) -> t) slotOrdered
   in printSlidingPuzzleSlots tiles

-- display the given puzzle slots content in rows of 4 tiles
printSlidingPuzzleSlots :: [Tile] -> String
printSlidingPuzzleSlots (c1:c2:c3:c4:r) =
  printf "%2d %2d %2d %2d\n%s" c1 c2 c3 c4 (printSlidingPuzzleSlots r)
printSlidingPuzzleSlots _ = ""
