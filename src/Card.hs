module Card where

import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.State
import           System.Random.Shuffle      (shuffleM)

data Card
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Show, Enum, Eq)

data Suit
  = Spade
  | Diamond
  | Club
  | Heart
  deriving (Show, Enum)

type Cards = [Card]

type Score = Int

-- | Shuffled 52 cards excluding Joker.
genShuffledDeck :: MonadRandom m => m Cards
genShuffledDeck = shuffleM . concat $ replicate 4 [Ace .. King]

toScore :: Card -> Score
toScore card =
  if card `elem` [Jack, Queen, King]
    then 10
    else 1 + fromEnum card

getScore :: Cards -> Score
getScore = sum . map toScore
