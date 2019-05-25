module Score
  ( Score(..)
  , toScore
  , best
  ) where

import           Card (Card, Rank (..))

data Score
  = Score [Int]
  | Bust
  deriving (Eq, Ord, Show)

toScore :: Card -> Score
toScore (_, card)
  | card == Ace = Score [1, 11]
  | card `elem` [Jack, Queen, King] = Score [10]
  | otherwise = Score [1 + fromEnum card]

best :: Score -> Score
best Bust          = Bust
best (Score score) = Score [maximum score]

instance Monoid Score where
  mempty = Score [0]

instance Semigroup Score where
  Bust <> (Score _) = Bust
  (Score _) <> Bust = Bust
  (Score n) <> (Score m)
    | null (n +: m) = Bust
    | otherwise = Score (n +: m)

(+:) :: [Int] -> [Int] -> [Int]
ns +: ms = filter (<= 21) [n + m | n <- ns, m <- ms]
