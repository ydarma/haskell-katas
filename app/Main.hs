module Main where

import           BowlingGame
import           MainArgs
import           System.Environment
import           Text.Printf

main :: IO ()
main = fmap run getArgs >>= putStrLn

run :: [String] -> String
run args =
  let (kata, kataArgs) = parseKata args
   in case kata of
        BowlingGameKata ->
          let score = runBowlingGame (parseBowlingGameArgs kataArgs)
           in printf "%d" score

runBowlingGame :: BowlingGameArgs -> Int
runBowlingGame args =
  let playedRolls = rolls args
      game = buildGame
      playedGame = playAll game playedRolls
   in score playedGame
