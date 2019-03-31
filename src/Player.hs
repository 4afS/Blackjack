module Player where

import           Card

data Player
  = Player { hand :: !Cards }
  | Dealer { hand :: !Cards }
  deriving (Show)

type Dealer = Player
