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
  (if w ^. clock & isResetting then go else w)
    & clock %~ tick clockTick
  where
    go =
      case w ^. state of
        GetReady -> updateGetReadyState w
        Playing -> updatePlayingState w
        Collision -> updateCollisionState w
        & keys .~ mempty

updateGetReadyState :: World -> World
updateGetReadyState w =
  if w ^. keys & NE.null & not
    then w & state .~ Playing
    else w

updateCollisionState :: World -> World
updateCollisionState w = w
