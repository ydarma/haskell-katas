module MainArgs where

-- bowling game command line arguments
newtype BowlingGameArgs = BowlingGameArgs
  { rolls :: [Int]
  }

-- parse command line arguments for the bowling game
parseBowlingGameArgs :: [String] -> BowlingGameArgs
parseBowlingGameArgs args =
  let rolls = map read args
   in BowlingGameArgs {rolls = rolls}

-- sliding puzzle command line arguments
newtype SlidingPuzzleArgs = SlidingPuzzleArgs
  { shuffleMoves :: Int
  }

-- parse command line arguments for the sliding puzzle
parseSlidingPuzzleArgs :: [String] -> SlidingPuzzleArgs
parseSlidingPuzzleArgs [shuffleMoves] = SlidingPuzzleArgs {shuffleMoves = read shuffleMoves}
parseSlidingPuzzleArgs _       = error "trailing arguments"

newtype PrimeNumbersArgs = PrimeNumbersArgs
  { upper :: Int
  }

parsePrimeNumbersArgs :: [String] -> PrimeNumbersArgs
parsePrimeNumbersArgs [upper] = PrimeNumbersArgs {upper = read upper}
parsePrimeNumbersArgs _       = error "trailing arguments"
