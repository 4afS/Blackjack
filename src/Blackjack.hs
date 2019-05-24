{-# LANGUAGE ViewPatterns #-}

module Blackjack
  ( playBlackjack
  ) where

import           Card
import           Control.Applicative
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.State
import           Debug.Trace
import           Player

data YesNo
  = Yes
  | No
  deriving (Eq)

data Result
  = Win
  | Draw
  | Lose
  deriving (Show)

playBlackjack :: IO ()
playBlackjack = do
  putStrLn ""
  shuffledDeck <- genShuffledDeck
  (hands, _) <- runStateT getHands shuffledDeck
  printHands hands
  printResult $ judge hands
  where
    printHands :: (Player, Player) -> IO ()
    printHands (Player pHand, Dealer dHand) = do
      putStrLn $ "Player's hand is " ++ show pHand
      putStrLn $ "Dealer's hand is " ++ show dHand
    printResult :: Result -> IO ()
    printResult result = putStrLn $ "Result : " ++ show result

getHands :: StateT Cards IO (Player, Player)
getHands = do
  dealer'sHand <- dealerTurn
  player'sHand <- playerTurn
  return (Player player'sHand, Dealer dealer'sHand)

drawCards :: Int -> StateT Cards IO Cards
drawCards n = do
  cards <- get
  put $ drop n cards
  return $ take n cards

drawCard :: StateT Cards IO Cards
drawCard = drawCards 1

dealerTurn :: StateT Cards IO Cards
dealerTurn = do
  cards <- get
  (handNumber, hand) <- drawCardsUntilOver17
  put $ drop handNumber cards
  lift . putStrLn $ "Dealer hand is " ++ show [head hand] ++ " and ..."
  return hand
  where
    drawCardsUntilOver17 :: StateT Cards IO (Int, Cards)
    drawCardsUntilOver17 = do
      cards <- get
      let dealerHand = drawUntilOver17 cards
      let dealerHandNumber = length dealerHand
      put $ drop dealerHandNumber cards
      return (dealerHandNumber, dealerHand)

    showDealerHand :: Cards -> StateT Cards IO ()
    showDealerHand hand = showHand "Dealer" hand (return . head)

drawUntilOver17 :: Cards -> Cards
drawUntilOver17 = tail . drawUntilOver17' (Score 0)
  where
    drawUntilOver17' Bust _ = []
    drawUntilOver17' (Score n) _ | n >= 17 = []
    drawUntilOver17' score (card:cards) = card : drawUntilOver17' (score <> (toScore card)) cards

playerTurn :: StateT Cards IO Cards
playerTurn = do
  cards <- get
  let initializedHand = take 2 cards
  showPlayerHand initializedHand
  askDrawMore
  yn <- askYesNo
  if yn == Yes
    then drawMore initializedHand
    else return initializedHand
  where
    drawMore :: Cards -> StateT Cards IO Cards
    drawMore nowHand = do
      hand <- (nowHand ++) <$> drawCard
      showPlayerHand hand
      askDrawMore
      yn <- askYesNo
      if yn == Yes
        then drawMore hand
        else return hand

    askDrawMore :: StateT a IO ()
    askDrawMore = lift $ putStrLn "Do you draw more? (y/n)"

    showPlayerHand :: Cards -> StateT Cards IO ()
    showPlayerHand hand = showHand "player" hand id

    askYesNo :: StateT Cards IO YesNo
    askYesNo = lift $ isYesOrNo <$> getLine

    isYesOrNo :: String -> YesNo
    isYesOrNo s
      | s `elem` ["Yes", "YES", "yes", "Y", "y"] = Yes
      | otherwise = No


showHand :: String -> Cards -> (Cards -> Cards) -> StateT Cards IO ()
showHand who hand operateToCards = lift . putStrLn $ who ++ " hand is " ++ show (operateToCards hand)

toScore :: Card -> Score
toScore card
  | card `elem` [Jack, Queen, King] = Score 10
  | otherwise = Score $ 1 + fromEnum card

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
