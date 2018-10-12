module Main where

import           BowlingGame
import           MainArgs
import           SlidingPuzzle
import           Data.List
import           System.Environment
import           System.Random
import           Text.Printf

main :: IO ()
main = fmap run getArgs >>= putStrLn

run :: [String] -> String
run ("BowlingGame":args)   = runBowlingGame (parseBowlingGameArgs args)
run ("SlidingPuzzle":args) = runSlidingPuzzle (parseSlidingPuzzleArgs args)
run _                      = error "uknown kata"

runBowlingGame :: BowlingGameArgs -> String
runBowlingGame args =
  let playedRolls = rolls args
      playedGame = playAll buildGame playedRolls
   in printf "score=%d" (score playedGame)

runSlidingPuzzle :: SlidingPuzzleArgs -> String
runSlidingPuzzle args =
  let tileMoves = moves args
      (shuffledPuzzle, _, _) = shuffle buildPuzzle tileMoves (mkStdGen 324)
      solution = bruteSolve shuffledPuzzle
   in printf
        "puzzle=\n%s\nmoves=%s"
        (printSlidingPuzzle shuffledPuzzle)
        (show $ bestMoves solution)

printSlidingPuzzle :: SlidingPuzzle -> String
printSlidingPuzzle puzzle =
  let slotTiles = zip ((emptySlot puzzle):(tileSlots puzzle)) [0..]
      ordered = sortBy (\(s1, t1) -> \(s2, t2) -> compare s1 s2) slotTiles
      tiles = map (\(s, t) -> t) ordered
      printRow :: [Int] -> String
      printRow (c1:c2:c3:c4:r) =
        printf "%2d %2d %2d %2d\n%s" c1 c2 c3 c4 (printRow r)
      printRow _ = ""
   in printRow tiles
