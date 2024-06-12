module Snake.Render.Segments
  ( renderSegmentsInterpolated,
    renderSegmentsAt,
  )
where

import Gloss.Extra.Clock (progress)
import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform (each, (&), (<&>), (^.), (^..))
import Snake.Geometry.V2 (PointF, V2 (V2), x, y)
import Snake.World (World, clock, segmentSize, segments)
import Snake.World.Segments (Segment (..), Segments, end, start, toList)

renderSegmentsInterpolated :: World -> Gloss.Picture
renderSegmentsInterpolated w =
  renderSegmentsAt (w ^. clock & progress) w

renderSegmentsAt :: Float -> World -> Gloss.Picture
renderSegmentsAt progress' w =
  let positions
        | progress' == 0 = toList segs ^.. each . start
        | progress' == 1 = toList segs ^.. each . end
        | otherwise = interpolatedPositions progress' segs
   in positions
        <&> renderBodySegment bodyColor segSize
        & Gloss.pictures
  where
    segs = w ^. segments
    bodyColor = Gloss.azure & Gloss.bright
    segSize = w ^. segmentSize

renderBodySegment :: Gloss.Color -> Float -> PointF -> Gloss.Picture
renderBodySegment bodyColor segSize segPos =
  Gloss.rectangleSolid segSize segSize
    & Gloss.color bodyColor
    & Gloss.translate (segPos ^. x) (segPos ^. y)

interpolatedPositions :: Float -> Segments -> [PointF]
interpolatedPositions t = fmap go . toList
  where
    go (Segment s e) = sinusoidalInterpolate t s e

sinusoidalInterpolate :: Float -> PointF -> PointF -> PointF
sinusoidalInterpolate t = linearInterpolate $ 0.5 * (1 - cos (t * pi))

linearInterpolate :: Float -> PointF -> PointF -> PointF
linearInterpolate t (V2 x1 y1) (V2 x2 y2) =
  V2 ((1 - t) * x1 + t * x2) ((1 - t) * y1 + t * y2)