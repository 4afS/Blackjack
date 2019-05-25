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

playPlayer :: StateT Deck IO Player
playPlayer = do
  initializedPlayer <- draw (Player [])
  lift $ print initializedPlayer
  yn <- lift askYesNo
  nextOrEnd yn initializedPlayer

nextOrEnd :: YesNo -> Player -> StateT Deck IO Player
nextOrEnd Yes player
  | getScore player == Bust = return player
  | otherwise = drawMore player
nextOrEnd No player = return player

drawMore :: Player -> StateT Deck IO Player
drawMore player = do
  playerDrew <- draw player
  lift . print $ playerDrew
  yn <- lift askYesNo
  nextOrEnd yn playerDrew

instance Players Player where
  getScore = foldl1 (<>) . map toScore . getHand
  draw player@(Player hand)
    | null hand = drawN player 2
    | otherwise = drawN player 1

instance Show Player where
  show (Player hand) = "Player : " ++ show hand

drawN :: Monad m => Player -> Int -> StateT Deck m Player
drawN (Player hand) n = do
  deck <- get
  put $ drop n deck
  return $ Player $ take n deck ++ hand

data YesNo
  = Yes
  | No

askYesNo :: IO YesNo
askYesNo = do
  putStrLn "Do you draw more? (y/n)"
  whichYesOrNo <$> getLine

whichYesOrNo :: String -> YesNo
whichYesOrNo s
  | s `elem` ["Yes", "YES", "yes", "Y", "y"] = Yes
  | otherwise = No
