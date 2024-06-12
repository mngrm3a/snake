module Snake.Render.Background (renderGrid) where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Graphics.Gloss as Gloss
import Snake.Geometry.Box (Box (_halfSize))
import Snake.Geometry.V2 (V2 (V2))
import Snake.World (World (_segmentSize, _window))

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
    segmentSize' = w & _segmentSize
    gridColor = Gloss.greyN 0.9
    V2 hw hh = w & _window & _halfSize