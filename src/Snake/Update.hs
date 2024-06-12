module Snake.Update (updateWorld) where

import qualified Data.Sequence as NE
import Gloss.Extra.Clock (isResetting, tick)
import Lens.Micro.Platform ((%~), (&), (.~), (^.))
import Snake.Update.Playing (updatePlayingState)
import Snake.World
  ( State (Collision, GetReady, Playing),
    World,
    clock,
    keys,
    state,
  )

updateWorld :: Float -> World -> World
updateWorld clockTick w =
  case w ^. state of
    GetReady -> updateGetReadyState w
    Playing -> updatePlayingState w
    Collision -> updateCollisionState w
    & keys %~ (if w ^. clock & isResetting then const mempty else id)
    & clock %~ tick clockTick

updateGetReadyState :: World -> World
updateGetReadyState w =
  if w ^. keys & NE.null & not
    then w & state .~ Playing
    else w

updateCollisionState :: World -> World
updateCollisionState w = w
