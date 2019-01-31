module Card where

data Card = Ace | Two | Tree | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show, Enum, Eq)

data Suit = Spade | Diamond | Club | Heart
    deriving (Show, Enum)

class Value a where
    toValue :: a -> Int