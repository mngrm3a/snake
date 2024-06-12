module Snake.Update.Collision (updateCollisionState) where

import qualified Data.Sequence as Seq
import Lens.Micro.Platform ((&), (.~), (^.))
import Snake.Geometry.Box (BoxF)
import qualified Snake.Geometry.Box as Box
import Snake.Geometry.V2 (PointF, V2 (V2), add, sub)
import Snake.World
  ( State (GetReady, Playing),
    World,
    keys,
    mkDefaultSegmentsAndVelocity,
    segmentSize,
    segments,
    state,
    velocity,
    window,
  )
import Snake.World.Segments (Segments, position)
import qualified Snake.World.Segments as Seg

updateCollisionState :: World -> World
updateCollisionState w = if canRecover w then recover else reset
  where
    recover =
      if w ^. keys & Seq.null & not
        then w & state .~ Playing
        else w
    reset =
      w
        & velocity .~ newVelocity
        & segments .~ newSegments
        & state .~ GetReady
    (newSegments, newVelocity) = mkDefaultSegmentsAndVelocity (w ^. segmentSize)

canRecover :: World -> Bool
canRecover w =
  not $ all (isCollision bBox segs) $ segmentNeighbors segSize segPos
  where
    segPos = w ^. segments & position
    segs = w ^. segments
    bBox = w ^. window
    segSize = w ^. segmentSize

segmentNeighbors :: Float -> PointF -> [PointF]
segmentNeighbors segSize pos =
  [add pos vx, sub pos vx, add pos vy, sub pos vy]
  where
    vx = V2 segSize 0
    vy = V2 0 segSize

isCollision :: BoxF -> Segments -> PointF -> Bool
isCollision bBox segs pos =
  not (Box.containsPoint bBox pos) || Seg.containsPoint pos segs