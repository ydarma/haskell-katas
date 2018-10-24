module MainArgs where

newtype BowlingGameArgs = BowlingGameArgs
  { rolls :: [Int]
  }

parseBowlingGameArgs :: [String] -> BowlingGameArgs
parseBowlingGameArgs args =
  let rolls = map read args
   in BowlingGameArgs {rolls = rolls}

newtype SlidingPuzzleArgs = SlidingPuzzleArgs
  { shuffleMoves :: Int
  }

parseSlidingPuzzleArgs :: [String] -> SlidingPuzzleArgs
parseSlidingPuzzleArgs [shuffleMoves] = SlidingPuzzleArgs {shuffleMoves = read shuffleMoves}
parseSlidingPuzzleArgs _       = error "trailing arguments"
