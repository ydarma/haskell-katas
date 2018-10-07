module MainArgs where

newtype BowlingGameArgs = BowlingGameArgs
  { rolls :: [Int]
  }

parseBowlingGameArgs :: [String] -> BowlingGameArgs
parseBowlingGameArgs args =
  let rolls = map read args
   in BowlingGameArgs {rolls = rolls}
