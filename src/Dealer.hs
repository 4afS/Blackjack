module Dealer
  ( Dealer(..)
  , playDealer
  ) where

import           Card                (Cards, Deck, showCards)
import           Control.Monad.State (StateT, get, lift, put)
import           Players             (Players (..))
import           Score               (Score (..), best, toScore)

data Dealer = Dealer
  { getHand :: !Cards
  }

playDealer :: StateT Deck IO Dealer
playDealer = do
  dealer <- draw (Dealer [])
  lift $ print $ only2Cards dealer
  return dealer

only2Cards :: Dealer -> Dealer
only2Cards (Dealer hand) = Dealer $ take 2 hand

instance Show Dealer where
  show (Dealer hand) = "Dealer : " ++ showCards hand

instance Players Dealer where
  getScore = foldl1 (<>) . map toScore . getHand
  draw _ = do
    deck <- get
    let hand = drawUntilOver17 deck
    put $ drop (length hand) deck
    return $ Dealer hand

drawUntilOver17 :: Deck -> Cards
drawUntilOver17 = drawUntilOver17' mempty
  where
    drawUntilOver17' :: Score -> Deck -> Cards
    drawUntilOver17' Bust _ = []
    drawUntilOver17' score (card:deck)
      | isOver17 score = []
      | otherwise = card : drawUntilOver17' (score <> toScore card) deck
    isOver17 :: Score -> Bool
    isOver17 score = best score >= Score [17]
