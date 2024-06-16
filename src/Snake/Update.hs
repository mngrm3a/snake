module Snake.Update (updateWorld) where

import qualified Data.Sequence as NE
import Lens.Micro.Platform ((&), (.~), (^.))
import Snake.Core.Clock (tick)
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
  let (hasReset, w') = tickClock w
   in if hasReset then w' & state .~ newWorldState w' else w'
  where
    tickClock w' =
      let (hasReset, newClock) = w' ^. state . clock & tick clockTick
       in (hasReset, w' & state . clock .~ newClock)
    newWorldState w' =
      case w' ^. state . status of
        GetReady -> updateGetReadyState we ws
        Playing -> updatePlayingState we ws & keys .~ mempty
        Collision -> updateCollisionState we ws
      where
        we = w' ^. env
        ws = w' ^. state

updateGetReadyState :: WorldEnv -> WorldState -> WorldState
updateGetReadyState _ ws =
  if ws ^. keys & NE.null & not
    then ws & status .~ Playing
    else ws
