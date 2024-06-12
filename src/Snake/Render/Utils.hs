module Snake.Render.Utils (renderOverlay, renderGrid) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&), (<&>), (^.))
import Snake.Geometry.Box (BoxF, center, halfSize, size)
import Snake.Geometry.V2 (V2 (V2))

renderOverlay :: Gloss.Color -> BoxF -> Gloss.Picture
renderOverlay color box =
  Gloss.rectangleSolid width height
    & Gloss.translate centerX centerY
    & Gloss.color color
  where
    V2 width height = box & size
    V2 centerX centerY = box ^. center

renderGrid :: Gloss.Color -> Float -> BoxF -> Gloss.Picture
renderGrid color segSize box =
  (horizontalPaths <> verticalPaths)
    <&> Gloss.line
    & Gloss.pictures
    & Gloss.color color
  where
    horizontalPaths =
      [ [(x + cX, y + cY), (x + cX + segSize, y + cY)]
        | x <- [(-hsW), (-hsW) + segSize .. hsW - segSize],
          y <- [(-hsH), (-hsH) + segSize .. hsH]
      ]
    verticalPaths =
      [ [(x + cX, y + cY), (x + cX, y + cY + segSize)]
        | x <- [(-hsW), (-hsW) + segSize .. hsW],
          y <- [(-hsH), (-hsH) + segSize .. hsH - segSize]
      ]
    V2 hsW hsH = box ^. halfSize
    V2 cX cY = box ^. center