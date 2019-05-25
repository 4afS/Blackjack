module Card
  ( Card(..)
  , Suit(..)
  , Cards
  , Deck
  , genShuffledDeck
  ) where

import           Control.Monad.Random.Class (MonadRandom)
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

type Deck = [Card]

-- | Shuffled 52 cards excluding Joker.
genShuffledDeck :: MonadRandom m => m Deck
genShuffledDeck = shuffleM deck
  where
    deck :: Deck
    deck = concat $ replicate 4 [Ace .. King]
