module Snake.Render.Segments
  ( renderSegmentsInterpolated,
    renderSegmentsAt,
  )
where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform (each, (&), (<&>), (^.), (^..))
import Snake.Core.Clock (progress)
import Snake.Geometry.V2 (PointF, V2 (V2), x, y)
import Snake.World (WorldEnv, WorldState, clock, segmentSize, segments)
import Snake.World.Segments (Segment (..), Segments, end, start, toList)

renderSegmentsInterpolated :: WorldEnv -> WorldState -> Gloss.Picture
renderSegmentsInterpolated we ws =
  renderSegmentsAt we ws (ws ^. clock & progress)

renderSegmentsAt :: WorldEnv -> WorldState -> Float -> Gloss.Picture
renderSegmentsAt we ws progress' =
  let positions
        | progress' == 0 = toList segs ^.. each . start
        | progress' == 1 = toList segs ^.. each . end
        | otherwise = interpolatedPositions progress' segs
   in positions
        <&> renderBodySegment bodyColor (we ^. segmentSize)
        & Gloss.pictures
  where
    segs = ws ^. segments
    bodyColor = Gloss.azure & Gloss.bright

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
