module Player
  ( Player(..)
  , playPlayer
  ) where

import           Card                (Cards, Deck)
import           Control.Monad.State (StateT, get, lift, put)
import           Players             (Players (..))
import           Score               (Score (..), toScore)

data Player = Player
  { getHand :: !Cards
  }

data YesNo
  = Yes
  | No

instance Show Player where
  show (Player hand) = "Player : " ++ show hand

instance Players Player where
  getScore = foldl (<>) (Score 0) . map toScore . getHand
  draw player@(Player hand)
    | null hand = drawN player 2
    | otherwise = drawN player 1

drawN :: Monad m => Player -> Int -> StateT Deck m Player
drawN (Player hand) n = do
  deck <- get
  put $ drop n deck
  return $ Player $ take n deck ++ hand

playPlayer :: StateT Cards IO Player
playPlayer = do
  initializedPlayer <- draw (Player [])
  lift $ print initializedPlayer
  yn <- lift askYesNo
  nextOrEnd yn initializedPlayer
  where
    nextOrEnd :: YesNo -> Player -> StateT Cards IO Player
    nextOrEnd Yes player
      | getScore player > Score 21 = return player
      | otherwise = drawMore player
    nextOrEnd No player = return player
    drawMore :: Player -> StateT Cards IO Player
    drawMore player = do
      playerDrew <- draw player
      lift . print $ playerDrew
      yn <- lift askYesNo
      nextOrEnd yn playerDrew

askYesNo :: IO YesNo
askYesNo = do
  putStrLn "Do you draw more? (y/n)"
  isYesOrNo <$> getLine
  where
    isYesOrNo :: String -> YesNo
    isYesOrNo s
      | s `elem` ["Yes", "YES", "yes", "Y", "y"] = Yes
      | otherwise = No
