module Snake.Core.Clock (Clock, mkClock, elapsed, progress, tick) where

import Data.Fixed (mod')

data Clock = Clock
  { _period :: !Float,
    _elapsed :: !Float,
    _progress :: !Float
  }
  deriving (Eq, Show)

mkClock :: Float -> Clock
mkClock p = Clock p p 1

elapsed :: Clock -> Float
elapsed = _elapsed

progress :: Clock -> Float
progress (Clock _ _ r) = r

tick :: Float -> Clock -> (Bool, Clock)
tick dt (Clock p e _) =
  ( isGreaterOrEqual,
    Clock
      { _period = p,
        _elapsed = if isGreaterOrEqual then remainder else e',
        _progress = if e' > p then remainder / p else e' / p
      }
  )
  where
    isGreaterOrEqual = e' >= p
    e' = e + dt
    remainder = e' `mod'` p
