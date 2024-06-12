module Snake.Update (updateWorld) where

import qualified Data.Sequence as NE
import Lens.Micro.Platform ((%~), (&), (.~))
import Snake.Update.Playing (updatePlayingState)
import Snake.World
  ( State (Collision, GetReady, Playing),
    World (_clock, _keys, _state),
    clock,
    isResetting,
    keys,
    state,
    tick,
  )

updateWorld :: Float -> World -> World
updateWorld clockTick w =
  case w & _state of
    GetReady -> updateGetReadyState w
    Playing -> updatePlayingState w
    Collision -> updateCollisionState w
    & keys %~ (if w & _clock & isResetting then const mempty else id)
    & clock %~ tick clockTick

updateGetReadyState :: World -> World
updateGetReadyState w =
  if w & _keys & NE.null & not
    then w & state .~ Playing
    else w

updateCollisionState :: World -> World
updateCollisionState w = w
