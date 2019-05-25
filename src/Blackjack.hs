module Blackjack
  ( playBlackjack
  ) where

import           Card                (Deck, genShuffledDeck)
import           Control.Monad.State (StateT, evalStateT)
import           Dealer              (Dealer, playDealer)
import           Player              (Player, playPlayer)
import           Result              (judge)

playBlackjack :: IO ()
playBlackjack = do
  lineBreak
  shuffledDeck <- genShuffledDeck
  (player, dealer) <- evalStateT play shuffledDeck
  lineBreak
  putStrLn "Result : "
  print dealer
  print player
  lineBreak
  print $ judge player dealer

lineBreak :: IO ()
lineBreak = putStrLn ""

play :: StateT Deck IO (Player, Dealer)
play = do
  dealer <- playDealer
  player <- playPlayer
  return (player, dealer)
