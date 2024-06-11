module Snake.Render.Segments
  ( renderSegmentEndPositions,
    renderInterpolatedSegmentPositions,
  )
where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&), (<&>))
import Snake.Geometry
import Snake.World
  ( Segment (Segment, _end),
    Segments,
    World (_clock, _segmentSize, _segments),
    progress,
    toList,
  )

renderInterpolatedSegmentPositions :: World -> Gloss.Picture
renderInterpolatedSegmentPositions w =
  interpolatedPositions clockProgress segs
    <&> renderBodySegment bodyColor segSize
    & Gloss.pictures
  where
    clockProgress = w & _clock & progress
    segs = w & _segments
    bodyColor = Gloss.azure & Gloss.bright
    segSize = w & _segmentSize

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

interpolatedPositions :: Float -> Segments -> [PointF]
interpolatedPositions t = fmap go . toList
  where
    go (Segment s e) = sinusoidalInterpolate t s e

sinusoidalInterpolate :: Float -> PointF -> PointF -> PointF
sinusoidalInterpolate t = linearInterpolate $ 0.5 * (1 - cos (t * pi))

linearInterpolate :: Float -> PointF -> PointF -> PointF
linearInterpolate t (V2 x1 y1) (V2 x2 y2) =
  V2 ((1 - t) * x1 + t * x2) ((1 - t) * y1 + t * y2)