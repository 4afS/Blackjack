module Blackjack (blackjack) where

import System.Random.Shuffle (shuffleM)
import Control.Monad.Random.Class (MonadRandom)

data Card = Ace | Two | Tree | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show, Enum, Eq)

-- data Suit = Spade | Diamond | Club | Heart

type Value = Int

-- data Trump = Trump 
--     {_card :: Card
--     ,_value :: Value
--     }

newtype Player = Player { _hand :: [Card] }

type Dealer = Player

data Action 
    = Hit
    | Stay

data Result 
    = Win
    | Draw
    | Lose

-- main = blackjack

-- | Shuffled 52 cards excluding Joker.
shuffledCards :: MonadRandom m => m [Card]
shuffledCards = shuffleM . concat $ replicate 4 [Ace .. King]

blackjack :: IO ()
blackjack = do
    print =<< shuffledCards
    print =<< length <$> shuffledCards


execute :: [a -> b] -> [a] -> [b]
execute [] _ = []
execute _ [] = []
execute (f:fs) (x:xs) = f x : execute fs xs

toValue card = 
    if card `elem` [Jack, Queen, King] 
       then 10 
       else 1 + fromEnum card

judge :: Player -> Dealer -> Result
judge = undefined
