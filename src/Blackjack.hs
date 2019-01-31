module Blackjack (blackjack) where

import System.Random.Shuffle (shuffleM)
import Control.Monad.Random.Class (MonadRandom)
import Card

newtype Player = Player { _hand :: [Card] }

type Dealer = Player

data Action 
    = Hit
    | Stay

data Result 
    = Win
    | Draw
    | Lose

-- | Shuffled 52 cards excluding Joker.
shuffledCards :: MonadRandom m => m [Card]
shuffledCards = shuffleM . concat $ replicate 4 [Ace .. King]

blackjack :: IO ()
blackjack = do
    print =<< shuffledCards
    print =<< length <$> shuffledCards

instance Value Card where
    toValue card = if card `elem` [Jack, Queen, King] then 10 else 1 + fromEnum card

judge :: Player -> Dealer -> Result
judge player dealer = undefined