module Dealer
  ( Dealer(..)
  , playDealer
  ) where

import           Card (Cards, Deck)
import           Control.Monad.State (StateT, lift, get, put)
import           Players (Players(..))
import           Score (Score(..), toScore)

data Dealer = Dealer
  { getHand :: !Cards
  }

playDealer :: StateT Cards IO Dealer
playDealer = do
  dealer <- draw (Dealer [])
  lift $ print dealer
  return dealer

instance Show Dealer where
  show (Dealer hand) = "Dealer : " ++ show hand

instance Players Dealer where
  getScore = foldl (<>) (Score 0) . map toScore . getHand
  draw _ = do
    deck <- get
    let hand = drawUntilOver17 deck
    put $ drop (length hand) deck
    return $ Dealer hand

drawUntilOver17 :: Deck -> Cards
drawUntilOver17 = drawUntilOver17' (Score 0)
  where
    drawUntilOver17' :: Score -> Deck -> Cards
    drawUntilOver17' Bust _ = []
    drawUntilOver17' (Score n) _
      | n >= 17 = []
    drawUntilOver17' score (card:cards) = card : drawUntilOver17' (score <> toScore card) cards
