module Card
  ( Rank(..)
  , Suit(..)
  , Card(..)
  , Cards
  , Deck
  , genShuffledDeck
  , showCards
  ) where

import           Control.Monad.Random.Class (MonadRandom)
import           System.Random.Shuffle      (shuffleM)

data Rank
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
  deriving (Eq, Enum)

instance Show Suit where
  show Spade = "S"
  show Diamond = "D"
  show Club = "C"
  show Heart = "H"

data Card = Card
  { suit :: Suit
  , rank :: Rank
  }

instance Show Card where
  show (Card suit rank) = "<" ++ show suit ++ ", " ++ show rank ++ ">"

showCards :: [Card] -> String
showCards [card] = show card
showCards (card:cards) = show card ++ ", " ++ showCards cards

type Cards = [Card]

type Deck = [Card]

-- | Shuffled 52 cards excluding Joker.
genShuffledDeck :: MonadRandom m => m Deck
genShuffledDeck = shuffleM deck

deck :: Deck
deck = spades ++ diamonds ++ clubs ++ hearts
  where
    spades, diamonds, clubs, hearts :: Cards
    spades = map (Card Spade) [Ace .. King]
    diamonds = map (Card Diamond) [Ace .. King]
    clubs = map (Card Club) [Ace .. King]
    hearts = map (Card Heart) [Ace .. King]
