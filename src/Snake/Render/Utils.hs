module Snake.Render.Utils (renderScreenOverlay) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&), (^.))
import Snake.Geometry.Box (size)
import Snake.Geometry.V2 (V2 (V2))
import Snake.World (World, window)

renderScreenOverlay :: Gloss.Color -> World -> Gloss.Picture
renderScreenOverlay overlayColor w =
  Gloss.rectangleSolid scrWidth scrHeight
    & Gloss.color overlayColor
  where
    V2 scrWidth scrHeight = w ^. window & size