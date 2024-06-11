module Snake.Render.Segments (renderSegmentEndPositions) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform
import Snake.Geometry
import Snake.World

renderSegmentEndPositions :: World -> Gloss.Picture
renderSegmentEndPositions w =
  endPositions segs
    <&> renderBodySegment bodyColor segSize
    & Gloss.pictures
  where
    bodyColor = Gloss.azure & Gloss.bright
    segSize = w & _segmentSize
    segs = w & _segments

renderBodySegment :: Gloss.Color -> Float -> PointF -> Gloss.Picture
renderBodySegment bodyColor segSize segPos =
  Gloss.rectangleSolid segSize segSize
    & Gloss.color bodyColor
    & Gloss.translate (segPos & _x) (segPos & _y)

endPositions :: Segments -> [PointF]
endPositions = fmap _end . toList
