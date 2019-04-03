module Blackjack
  ( playBlackjack
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
  (numberOfCards, drewCards) <- drawCardsWhileScoreOver17
  put $ drop numberOfCards cards
  lift . putStrLn $ "Dealer hand is " ++ show [head drewCards] ++ " and ..."
  return drewCards
  where
    drawCardsWhileScoreOver17 :: StateT Cards IO (Int, Cards)
    drawCardsWhileScoreOver17 = do
      cards <- get
      let over17Index = getOver17Index cards
      put $ drop over17Index cards
      return (over17Index, take over17Index cards)
      where
        getOver17Index = length . takeWhile (<= Score 17) . scanl (<>) (Score 0) . map toScore

    showDealerHand :: Cards -> StateT Cards IO ()
    showDealerHand hand = showHand "Dealer" hand ((:[]) . head)

playerTurn :: StateT Cards IO Cards
playerTurn = do
  cards <- get
  let initialDrewCards = take 2 cards
  showPlayerHand initialDrewCards
  askDrawMore
  yn <- yesNoQuestion
  if yn == Yes
    then drawMore initialDrewCards
    else return initialDrewCards
  where
    drawMore :: Cards -> StateT Cards IO Cards
    drawMore nowHand = do
      drewCards <- (nowHand ++) <$> drawCard
      showPlayerHand drewCards
      askDrawMore
      yn <- yesNoQuestion
      if yn == Yes
        then drawMore drewCards
        else return drewCards

    askDrawMore :: StateT a IO ()
    askDrawMore = lift $ putStrLn "Do you draw more? (y/n)"

    showPlayerHand :: Cards -> StateT Cards IO ()
    showPlayerHand hand = showHand "player" hand id

    yesNoQuestion :: StateT Cards IO YesOrNo
    yesNoQuestion = lift $ isYesOrNo <$> getLine

    isYesOrNo :: String -> YesOrNo
    isYesOrNo s
      | s `elem` ["Yes", "YES", "yes", "Y", "y"] = Yes
      | otherwise = No


showHand :: String -> Cards -> (Cards -> Cards) -> StateT Cards IO ()
showHand who hand operateToCards = lift . putStrLn $ who ++ " hand is " ++ show (operateToCards hand)

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
