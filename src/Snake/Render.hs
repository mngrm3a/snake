module Snake.Render (renderWorld) where

import qualified Graphics.Gloss as Gloss
import Snake.Render.Background (renderGrid)
import Snake.World (World)

renderWorld :: World -> Gloss.Picture
renderWorld = renderGrid