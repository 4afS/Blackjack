module Blackjack
  ( blackjack
  ) where

import           Card
import           Control.Applicative
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.State
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
  dealer'sHand <- drawCardsByDealer
  player'sHand <- drawCardsByPlayer
  return (Player player'sHand, Dealer dealer'sHand)

drawCards :: Int -> StateT Cards IO Cards
drawCards n = do
  cards <- get
  put $ drop n cards
  return $ take n cards

drawCardsByDealer :: StateT Cards IO Cards
drawCardsByDealer = do
  cards <- get
  (numberOfCards, drewCards) <- takeCardsWhileSumValueIsNotLessThan17
  put $ drop numberOfCards cards
  lift $ putStrLn $ "Dealer's hand are " ++ show (head drewCards) ++ " and more."
  return drewCards
  where
    takeCardsWhileSumValueIsNotLessThan17 :: StateT Cards IO (Int, Cards)
    takeCardsWhileSumValueIsNotLessThan17 = do
      cards <- get
      let index = length . takeWhile (<= 17) . scanl (+) 0 $ map toScore cards
      put $ drop index cards
      return (index, take index cards)

drawCardsByPlayer :: StateT Cards IO Cards
drawCardsByPlayer = do
  cards <- get
  lift . putStrLn $ "you got " ++ show (take 2 cards)
  lift $ putStrLn "do you draw more? (y/n) : "
  yn <- lift $ yesOrNo <$> getLine
  if yn == Yes
    then drawCards 3
    else drawCards 2
  where
    yesOrNo :: String -> YesOrNo
    yesOrNo s
      | s `elem` ["Yes", "YES", "yes", "Y", "y"] = Yes
      | s `elem` ["No", "no", "n", "N"] = No

judge :: (Player, Dealer) -> Result
judge (player, dealer)
  | (playerHandsScore > 21) && (dealerHandsScore > 21) = Draw
  | playerHandsScore > 21 = Lose
  | dealerHandsScore > 21 = Win
  | playerHandsScore > dealerHandsScore = Win
  | playerHandsScore < dealerHandsScore = Lose
  | otherwise = Draw
  where
    playerHandsScore = getScore (hand player)
    dealerHandsScore = getScore (hand dealer)
