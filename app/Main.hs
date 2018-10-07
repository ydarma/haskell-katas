module Main where

import           BowlingGame
import           MainArgs
import           System.Environment
import           Text.Printf

main :: IO ()
main = fmap run getArgs >>= putStrLn

run :: [String] -> String
run ("BowlingGame":args) = runBowlingGame (parseBowlingGameArgs args)
run _                    = error "uknown kata"

runBowlingGame :: BowlingGameArgs -> String
runBowlingGame args =
  let playedRolls = rolls args
      game = buildGame
      playedGame = playAll game playedRolls
   in printf "socre=%d" (score playedGame)
