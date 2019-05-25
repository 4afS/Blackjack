module Result
  ( Result(..)
  , judge
  ) where

import           Dealer  (Dealer)
import           Player  (Player)
import           Players (Players (..))
import           Score   (Score (..), best)

data Result
  = Win
  | Draw
  | Lose

judge :: Player -> Dealer -> Result
judge player dealer = judge' playerScore dealerScore
  where
    playerScore, dealerScore :: Score
    playerScore = best $ getScore player
    dealerScore = best $ getScore dealer
    judge' :: Score -> Score -> Result
    judge' Bust Bust = Draw
    judge' (Score _) Bust = Win
    judge' Bust (Score _) = Lose
    judge' (Score [playerScore]) (Score [dealerScore])
      | playerScore == dealerScore = Draw
      | playerScore > dealerScore = Win
      | playerScore < dealerScore = Lose

instance Show Result where
  show Win  = "Player Win :)"
  show Draw = "Player Draw :|"
  show Lose = "Player Lose ;("
