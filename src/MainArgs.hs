module MainArgs where

data Kata =
  BowlingGameKata
  deriving (Eq, Show)

newtype BowlingGameArgs = BowlingGameArgs
  { rolls :: [Int]
  }

parseKata :: [String] -> (Kata, [String])
parseKata ("BowlingGame":args) = (BowlingGameKata, args)
parseKata _                    = error "unknown kata"

parseBowlingGameArgs :: [String] -> BowlingGameArgs
parseBowlingGameArgs args =
  let rolls = map read args
   in BowlingGameArgs {rolls = rolls}
