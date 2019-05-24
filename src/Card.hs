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

data Score
  = Score Int
  | Bust
  deriving (Eq, Ord, Show)

instance Semigroup Score where
  Bust <> (Score _) = Bust
  (Score _) <> Bust = Bust
  (Score n) <> (Score m)
    | n + m > 21 = Bust
    | otherwise = Score (n + m)

-- | Shuffled 52 cards excluding Joker.
genShuffledDeck :: MonadRandom m => m Cards
genShuffledDeck = shuffleM cards
  where
    cards = concat $ replicate 4 [Ace .. King]

