module BowlingGame
  ( makeGame
  , playAll
  , play
  , score
  , rollOffset
  ) where

-- a game is described by the sequence of turns and the score
data BowlingGame = BowlingGame
  { score :: Int
  , turns :: [Turn]
  } deriving (Show)

-- a turn is composed with one or two rolls, it may be incomplete
data Turn
  = FirstRoll { first :: Roll }
  | TwoRolls { first  :: Roll
             , second :: Roll }
  | Spare { first :: Roll }
  | Strike
  deriving (Show)

-- number of skittles hit in a roll
type Roll = Int

-- make a new game
makeGame :: BowlingGame
makeGame = BowlingGame {score = 0, turns = []}

-- play all the given rolls and return an updated game
playAll :: BowlingGame -> [Roll] -> BowlingGame
playAll game []    = game
playAll game rolls = foldl play game rolls

-- play the given roll and return an updated game
play :: BowlingGame -> Roll -> BowlingGame
play game skittles =
  let newScore = increaseScore game skittles
      newTurns = addRoll game skittles
   in BowlingGame {score = newScore, turns = newTurns}

-- increase game score with the given roll, return the new score
increaseScore :: BowlingGame -> Roll -> Int
increaseScore game skittles =
  let currentScore = score game
      (currentTurn, _) = rollOffset game
      playedTurns = turns game
   in case playedTurns of
        (Strike:Strike:_) -> twoStrikes currentTurn skittles + currentScore
        (Strike:_) -> oneStrikeOrSpare currentTurn skittles + currentScore
        (_:Strike:_) -> oneStrikeOrSpare currentTurn skittles + currentScore
        (Spare _:_) -> oneStrikeOrSpare currentTurn skittles + currentScore
        _ -> noStrikeOrSpare currentTurn skittles + currentScore

-- compute score increment after two consecutive strikes
twoStrikes :: Int -> Roll -> Int
twoStrikes currentTurn skittles
  | currentTurn < 10 = 3 * skittles
  | currentTurn < 11 = 2 * skittles
  | currentTurn < 12 = skittles
  | otherwise = error "game ended"

-- compute score increment after a single strike or a spare
oneStrikeOrSpare :: Int -> Roll -> Int
oneStrikeOrSpare currentTurn skittles
  | currentTurn < 10 = 2 * skittles
  | currentTurn < 11 = skittles
  | otherwise = error "game ended"

-- compute score increment after two normal rolls
noStrikeOrSpare :: Int -> Roll -> Int
noStrikeOrSpare currentTurn skittles
  | currentTurn < 10 = skittles
  | otherwise = error "game ended"

-- get the moment of the game as (played turns, played rolls in current turn)
rollOffset :: BowlingGame -> (Int, Int)
rollOffset game =
  case turns game of
    (FirstRoll _:previous) -> (length previous, 1)
    all                    -> (length all, 0)

-- update game turns with the given roll, return the new turns
addRoll :: BowlingGame -> Roll -> [Turn]
addRoll game skittles =
  case turns game of
    (FirstRoll first:playedTurns) -> secondRoll first skittles : playedTurns
    playedTurns                   -> firstRoll skittles : playedTurns

-- build turn after first roll
firstRoll :: Roll -> Turn
firstRoll skittles
  | skittles == 10 = Strike
  | isValidRoll skittles = FirstRoll {first = skittles}
  | otherwise = error "invalid roll"

-- build turn after second roll
secondRoll :: Roll -> Roll -> Turn
secondRoll first skittles
  | first + skittles == 10 = Spare {first = first}
  | isValidRoll (first + skittles) =
    TwoRolls {first = first, second = skittles}
  | otherwise = error "invalid roll"

-- check if a roll is valid
isValidRoll :: Int -> Bool
isValidRoll skittles = skittles >= 0 && skittles <= 10
