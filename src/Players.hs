module Players
  ( Players(..)
  ) where

import           Card                (Cards, Deck)
import           Control.Monad.State (StateT)
import           Score               (Score)

class Players a where
  getScore :: a -> Score
  draw :: Monad m => a -> StateT Deck m a
