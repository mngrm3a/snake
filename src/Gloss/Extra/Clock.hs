module Gloss.Extra.Clock (Clock, mkClock, isResetting, progress, tick) where

data Clock = Clock !Float !Float deriving (Eq, Show)

mkClock :: Float -> Clock
mkClock f = Clock f f

isResetting :: Clock -> Bool
isResetting (Clock f v) = v >= f

progress :: Clock -> Float
progress (Clock f v) = v / f

tick :: Float -> Clock -> Clock
tick dt (Clock f v) = Clock f $ if v >= f then dt else v + dt
