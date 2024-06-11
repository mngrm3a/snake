module Snake.Update (updateWorld) where

import Lens.Micro.Platform ((%~), (&))
import Snake.World (State (..), World (_clock, _state), clock, progress, tick)

updateWorld :: Float -> World -> World
updateWorld clockTick w =
  case w & _state of
    GetReady -> updateGetReadyState clockProgress w
    Playing -> updatePlayingState clockProgress w
    Collision -> updateCollisionState clockProgress w
    & clock %~ tick clockTick
  where
    clockProgress = w & _clock & progress

updateGetReadyState :: Float -> World -> World
updateGetReadyState _ w = w

updatePlayingState :: Float -> World -> World
updatePlayingState _ w = w

updateCollisionState :: Float -> World -> World
updateCollisionState _ w = w
