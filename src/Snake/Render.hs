module Snake.Render (renderWorld) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&))
import Snake.Render.Background (renderGrid)
import Snake.Render.Utils (renderScreenOverlay)
import Snake.World (State (Collision, GetReady, Playing), World (_state))

renderWorld :: World -> Gloss.Picture
renderWorld w =
  renderBackground w
    <> case w & _state of
      GetReady -> renderGetReadyState w
      Playing -> renderPlayingState w
      Collision -> renderCollisionState w

renderBackground :: World -> Gloss.Picture
renderBackground = renderGrid

renderGetReadyState :: World -> Gloss.Picture
renderGetReadyState = renderScreenOverlay overlayColor
  where
    overlayColor = Gloss.greyN 0.8 & Gloss.withAlpha 0.65

renderPlayingState :: World -> Gloss.Picture
renderPlayingState = const Gloss.blank

renderCollisionState :: World -> Gloss.Picture
renderCollisionState = const Gloss.blank
