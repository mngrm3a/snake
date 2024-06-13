module Snake.Update (updateWorld) where

import qualified Data.Sequence as NE
import Gloss.Extra.Clock (isResetting, tick)
import Lens.Micro.Platform ((%~), (&), (.~), (^.))
import Snake.Update.Collision (updateCollisionState)
import Snake.Update.Playing (updatePlayingState)
import Snake.World
  ( Status (..),
    World,
    WorldEnv,
    WorldState,
    clock,
    env,
    keys,
    state,
    status,
  )

updateWorld :: Float -> World -> World
updateWorld clockTick w =
  (if w ^. state . clock & isResetting then w & state .~ newWorldState else w)
    & state . clock %~ tick clockTick
  where
    newWorldState =
      case w ^. state . status of
        GetReady -> updateGetReadyState we ws
        Playing -> updatePlayingState we ws & keys .~ mempty
        Collision -> updateCollisionState we ws
    we = w ^. env
    ws = w ^. state

updateGetReadyState :: WorldEnv -> WorldState -> WorldState
updateGetReadyState _ ws =
  if ws ^. keys & NE.null & not
    then ws & status .~ Playing
    else ws
