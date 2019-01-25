module Blackjack (blackjack) where

data Card = Ace | Two | Tree | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show, Enum, Eq)

type Value = Int

data Trump = Trump {
    _card :: Card,
    _value :: Value
    } deriving (Eq, Show)

newtype Player = Player { _trumps :: [Trump] }

data Action 
    = Hit
    | Stay

data Result 
    = Win
    | Draw
    | Lose

class Trumps a where
    score :: [a] -> Value 
    cards :: [a] -> [Card]

instance Trumps Trump where
    score = sum . map _value
    cards = map _card

blackjack :: IO ()
blackjack = undefined

main = do
    let me  = Player [Trump Ace 1, Trump Jack 10, Trump Five 5]
        dealer  = Player [Trump Two 2, Trump King 10, Trump Seven 7]
    print $ score $ _trumps me
    print $ cards $ _trumps me
    print $ cards $ _trumps dealer 

    let cards = [Ace .. King]

    print $ execute [Trump card | card <- cards] [1..]

execute :: [a -> b] ->[a] -> [b]
execute [] _ = []
execute _ [] = []
execute (f:fs) (x:xs) = f x : execute fs xs


