module Snake.World.Grid (gridCenterPoints, gridCenterPoints') where

import Lens.Micro.Platform ((^.))
import Snake.Geometry.Box (Box, center, halfSize)
import Snake.Geometry.V2 (V2 (V2))

gridCenterPoints :: (Fractional s, Enum s) => Box s -> s -> [V2 s]
gridCenterPoints box segmentSize = gridCenterPoints' box $ V2 segmentSize segmentSize

gridCenterPoints' :: (Fractional s, Enum s) => Box s -> V2 s -> [V2 s]
gridCenterPoints' box segmentSize =
  [ V2 (x + segHw) (y + segHh)
    | x <- [leftE, leftE + segW .. rightE - segW],
      y <- [bottomE, bottomE + segH .. topE - segH]
  ]
  where
    V2 boxCx boxCy = box ^. center
    V2 boxHw boxHh = box ^. halfSize
    V2 segW segH = segmentSize
    (segHw, segHh) = (segW / 2, segH / 2)
    (leftE, rightE) = (boxCx - boxHw, boxCx + boxHw)
    (bottomE, topE) = (boxCy - boxHh, boxCy + boxHh)