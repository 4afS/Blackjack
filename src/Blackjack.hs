module Blackjack
  ( blackjack
  ) where

import           Card
import           Control.Applicative
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.State
import           Debug.Trace
import           Player

data YesOrNo
  = Yes
  | No
  deriving (Eq)

data Result
  = Win
  | Draw
  | Lose
  deriving (Show)

blackjack :: IO ()
blackjack = do
  putStrLn ""
  shuffledDeck <- genShuffledDeck
  (hands, _) <- runStateT playBlackjack shuffledDeck
  printHands hands
  printResult $ judge hands
  where
    printHands :: (Player, Player) -> IO ()
    printHands (Player pHand, Dealer dHand) = do
      putStrLn $ "Player's hand are " ++ show pHand
      putStrLn $ "Dealer's hand are " ++ show dHand
    printResult :: Result -> IO ()
    printResult result = putStrLn $ "Result : " ++ show result

playBlackjack :: StateT Cards IO (Player, Player)
playBlackjack = do
  dealer'sHand <- dealersTurn
  player'sHand <- playersTurn
  return (Player player'sHand, Dealer dealer'sHand)

drawCards :: Int -> StateT Cards IO Cards
drawCards n = do
  cards <- get
  put $ drop n cards
  return $ take n cards

drawCard :: StateT Cards IO Cards
drawCard = drawCards 1

dealersTurn :: StateT Cards IO Cards
dealersTurn = do
  cards <- get
  (numberOfCards, drewCards) <- drawCardsWhileScoreIsNotLessThan17
  put $ drop numberOfCards cards
  lift . putStrLn $ "Dealer's hand are " ++ show [head drewCards] ++ " and more."
  return drewCards
  where
    drawCardsWhileScoreIsNotLessThan17 :: StateT Cards IO (Int, Cards)
    drawCardsWhileScoreIsNotLessThan17 = do
      cards <- get
      let index = length . takeWhile (<= Score 17) . scanl (<>) (Score 0) $ map toScore cards
      put $ drop index cards
      return (index, take index cards)

playersTurn :: StateT Cards IO Cards
playersTurn = do
  cards <- get
  let initialDrewCards = take 2 cards
  showHand initialDrewCards
  lift $ putStrLn "do you draw more? (y/n)"
  yn <- yesNoQuestion
  if yn == Yes
    then drawMore initialDrewCards
    else return initialDrewCards
  where
    showHand :: Cards -> StateT Cards IO ()
    showHand hand = lift . putStrLn $ "your hand is " ++ show hand

    yesNoQuestion :: StateT Cards IO YesOrNo
    yesNoQuestion = lift $ isYesOrNo <$> getLine

    isYesOrNo :: String -> YesOrNo
    isYesOrNo s
      | s `elem` ["Yes", "YES", "yes", "Y", "y"] = Yes
      | otherwise = No

    drawMore :: Cards -> StateT Cards IO Cards
    drawMore nowHand = do
      drewCards <- (nowHand ++) <$> drawCard
      showHand drewCards
      lift $ putStrLn "do you draw more? (y/n)"
      yn <- yesNoQuestion
      if yn == Yes
        then drawMore drewCards
        else return drewCards

toScore :: Card -> Score
toScore card =
  if card `elem` [Jack, Queen, King]
    then Score 10
    else Score $ 1 + fromEnum card

getTotalScore :: Player -> Score
getTotalScore = foldl (<>) (Score 0) . map toScore . hand

judge :: (Player, Dealer) -> Result
judge (player, dealer) =
  let player'sScore = getTotalScore player
      dealer'sScore = getTotalScore dealer
   in judge' player'sScore dealer'sScore
  where
    judge' Bust Bust = Draw
    judge' (Score _) Bust = Win
    judge' Bust (Score _) = Lose
    judge' (Score pScore) (Score dScore)
      | pScore == dScore = Draw
      | pScore > dScore = Win
      | pScore < dScore = Lose
