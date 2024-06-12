module Snake.Render.Background (renderGrid) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&), (<&>), (^.))
import Snake.Geometry.Box (halfSize)
import Snake.Geometry.V2 (V2 (V2))
import Snake.World (World, segmentSize, window)

renderGrid :: World -> Gloss.Picture
renderGrid w =
  (horizontalPaths <> verticalPaths)
    <&> Gloss.line
    & Gloss.pictures
    & Gloss.color gridColor
  where
    horizontalPaths =
      [ [(x', y'), (x' + segmentSize', y')]
        | x' <- [(-hw), (-hw) + segmentSize' .. hw - segmentSize'],
          y' <- [(-hh), (-hh) + segmentSize' .. hh]
      ]
    verticalPaths =
      [ [(x', y'), (x', y' + segmentSize')]
        | x' <- [(-hw), (-hw) + segmentSize' .. hw],
          y' <- [(-hh), (-hh) + segmentSize' .. hh - segmentSize']
      ]
    segmentSize' = w ^. segmentSize
    gridColor = Gloss.greyN 0.9
    V2 hw hh = w ^. window . halfSize