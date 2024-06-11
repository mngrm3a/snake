module Snake.Event (handleEvent) where

import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform ((&), (.~))
import Snake.World (State (..), World (_state), state)

handleEvent :: Gloss.Event -> World -> World
handleEvent event@(Gloss.EventKey {}) w =
  case w & _state of
    GetReady -> handleGetReadyState event w
    Playing -> handlePlayingState event w
    Collision -> handleCollisionState event w
handleEvent _ w = w

handleGetReadyState :: Gloss.Event -> World -> World
handleGetReadyState _ w = w & state .~ Playing

handlePlayingState :: Gloss.Event -> World -> World
handlePlayingState _ w = w

handleCollisionState :: Gloss.Event -> World -> World
handleCollisionState _ w = w