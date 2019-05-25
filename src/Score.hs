module Score
  ( Score(..)
  , toScore
  , best
  , printScore
  ) where

import           Card (Card, Rank (..))

data Score
  = Score [Int]
  | Bust
  deriving (Eq, Ord, Show)

toScore :: Card -> Score
toScore (_, Ace) = Score [1, 11]
toScore (_, card)
  | card `elem` [Jack, Queen, King] = Score [10]
  | otherwise = Score [1 + fromEnum card]

best :: Score -> Score
best Bust          = Bust
best (Score score) = Score [maximum score]

printScore :: Score -> IO ()
printScore Bust = putStrLn "Score >> Bust"
printScore score = putStrLn $ "Score >> " ++ show (getBestScoreValue score)
  where
    getBestScoreValue :: Score -> Int
    getBestScoreValue = getValue . best
    getValue :: Score -> Int
    getValue (Score (value:_)) = value

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
