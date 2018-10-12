module MainArgs where

newtype BowlingGameArgs = BowlingGameArgs
  { rolls :: [Int]
  }

parseBowlingGameArgs :: [String] -> BowlingGameArgs
parseBowlingGameArgs args =
  let rolls = map read args
   in BowlingGameArgs {rolls = rolls}

newtype SlidingPuzzleArgs = SlidingPuzzleArgs
  { moves :: Int
  }

parseSlidingPuzzleArgs :: [String] -> SlidingPuzzleArgs
parseSlidingPuzzleArgs [moves] = SlidingPuzzleArgs {moves = read moves}
parseSlidingPuzzleArgs _       = error "trailing arguments"
