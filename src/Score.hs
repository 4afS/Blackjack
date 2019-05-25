module Score
  ( Score(..)
  , toScore
  ) where

import           Card (Card, Rank (..))

data Score
  = Score Int
  | Bust
  deriving (Eq, Ord)

instance Semigroup Score where
  Bust <> (Score _) = Bust
  (Score _) <> Bust = Bust
  (Score n) <> (Score m)
    | n + m > 21 = Bust
    | otherwise = Score (n + m)

toScore :: Card -> Score
toScore (_, card)
  | card `elem` [Jack, Queen, King] = Score 10
  | otherwise = Score $ 1 + fromEnum card
