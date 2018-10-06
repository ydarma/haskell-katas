module BowlingGame
  ( buildGame
  , playAll
  , play
  , score
  , turn
  ) where

data Turn
  = FirstRoll { first :: Int }
  | TwoRolls { first  :: Int
             , second :: Int }
  | Spare { first :: Int }
  | Strike
  deriving (Show)

data BowlingGame = BowlingGame
  { score :: Int
  , turns :: [Turn]
  } deriving (Show)

buildGame :: BowlingGame
buildGame = BowlingGame {score = 0, turns = []}

playAll :: BowlingGame -> [Int] -> BowlingGame
playAll game []    = game
playAll game turns = foldl play game turns

play :: BowlingGame -> Int -> BowlingGame
play game skittles =
  let newScore = increaseScore game skittles
      newTurns = addTurn (turns game) skittles
   in BowlingGame {score = newScore, turns = newTurns}

increaseScore :: BowlingGame -> Int -> Int
increaseScore game skittles =
  let currentScore = score game
      (currentTurn, _) = turn game
      twoStrikes
        | currentTurn < 10 = currentScore + 3 * skittles
        | currentTurn < 11 = currentScore + 2 * skittles
        | currentTurn < 12 = currentScore + skittles
        | otherwise = error "game ended"
      oneStrikeOrSpare
        | currentTurn < 10 = currentScore + 2 * skittles
        | currentTurn < 11 = currentScore + skittles
        | otherwise = error "game ended"
      noStrikeOrSpare
        | currentTurn < 10 = currentScore + skittles
        | otherwise = error "game ended"
   in case turns game of
        (Strike:Strike:_) -> twoStrikes
        (Strike:_)        -> oneStrikeOrSpare
        (_:Strike:_)      -> oneStrikeOrSpare
        (Spare _:_)       -> oneStrikeOrSpare
        _                 -> noStrikeOrSpare

turn :: BowlingGame -> (Int, Int)
turn game =
  case turns game of
    (FirstRoll _:previous) -> (length previous, 1)
    all                -> (length all, 0)

addTurn :: [Turn] -> Int -> [Turn]
addTurn turns skittles =
  let addSecondRoll :: Int -> [Turn] -> [Turn]
      addSecondRoll first previous
        | first + skittles == 10 = Spare {first = first} : previous
        | isValidRoll (first + skittles) =
          TwoRolls {first = first, second = skittles} : previous
        | otherwise = error "invalid roll"
      addFirstRoll :: [Turn] -> [Turn]
      addFirstRoll previous
        | skittles == 10 = Strike : previous
        | isValidRoll skittles = FirstRoll {first = skittles} : previous
        | otherwise = error "invalid roll"
   in case turns of
        (FirstRoll first:previous) -> addSecondRoll first previous
        previous                   -> addFirstRoll previous

isValidRoll :: Int -> Bool
isValidRoll skittles = skittles >= 0 && skittles <= 10
