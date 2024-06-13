module Snake.Update.Collision (updateCollisionState) where

import qualified Data.Sequence as Seq
import Lens.Micro.Platform ((&), (.~), (^.))
import Snake.Geometry.Box (BoxF)
import qualified Snake.Geometry.Box as Box
import Snake.Geometry.V2 (PointF, V2 (V2), add, sub)
import Snake.World
  ( Status (Playing),
    WorldEnv,
    WorldState,
    keys,
    lives,
    mkWorldStateFromWorldEnv,
    segmentSize,
    segments,
    status,
    window,
  )
import Snake.World.Segments (Segments, position)
import qualified Snake.World.Segments as Seg

updateCollisionState :: WorldEnv -> WorldState -> WorldState
updateCollisionState we ws =
  if canRecover we ws && ws ^. lives > 0
    then
      if ws ^. keys & Seq.null & not
        then ws & status .~ Playing
        else ws
    else mkWorldStateFromWorldEnv we

canRecover :: WorldEnv -> WorldState -> Bool
canRecover we ws =
  not $ all (isCollision bBox segs) $ segmentNeighbors segSize segPos
  where
    segPos = ws ^. segments & position
    segs = ws ^. segments
    bBox = we ^. window
    segSize = we ^. segmentSize

segmentNeighbors :: Float -> PointF -> [PointF]
segmentNeighbors segSize pos =
  [add pos vx, sub pos vx, add pos vy, sub pos vy]
  where
    vx = V2 segSize 0
    vy = V2 0 segSize

isCollision :: BoxF -> Segments -> PointF -> Bool
isCollision bBox segs pos =
  not (Box.containsPoint bBox pos) || Seg.containsPoint pos segs