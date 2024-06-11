module Snake.Update (updateWorld) where

import qualified Data.Sequence as NE
import Lens.Micro.Platform ((%~), (&), (.~))
import Snake.World
  ( State (..),
    World (_clock, _keys, _state),
    clock,
    keys,
    progress,
    state,
    tick,
  )

updateWorld :: Float -> World -> World
updateWorld clockTick w =
  case w & _state of
    GetReady -> updateGetReadyState clockProgress w
    Playing -> updatePlayingState clockProgress w
    Collision -> updateCollisionState clockProgress w
    & keys .~ mempty
    & clock %~ tick clockTick
  where
    clockProgress = w & _clock & progress

updateGetReadyState :: Float -> World -> World
updateGetReadyState _ w =
  if not $ NE.null $ w & _keys
    then w & state .~ Playing
    else w

updatePlayingState :: Float -> World -> World
updatePlayingState _ w = w

updateCollisionState :: Float -> World -> World
updateCollisionState _ w = w
