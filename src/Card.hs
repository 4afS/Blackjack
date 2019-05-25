{-# LANGUAGE TupleSections #-}

module Card
  ( Rank(..)
  , Suit(..)
  , Card
  , Cards
  , Deck
  , genShuffledDeck
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
  deriving (Show, Enum)

type Card = (Suit, Rank)

type Cards = [Card]

type Deck = [Card]

-- | Shuffled 52 cards excluding Joker.
genShuffledDeck :: MonadRandom m => m Deck
genShuffledDeck = shuffleM deck

deck :: Deck
deck = spades ++ diamonds ++ clubs ++ hearts
  where
    spades, diamonds, clubs, hearts :: Cards
    spades = map (Spade, ) [Ace .. King]
    diamonds = map (Diamond, ) [Ace .. King]
    clubs = map (Club, ) [Ace .. King]
    hearts = map (Heart, ) [Ace .. King]
