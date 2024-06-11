{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Snake.World.Segments
  ( Segments,
    Segment (..),
    start,
    end,
    mkSegments,
    toList,
    moveTo,
    extendTo,
    position,
    containsPoint,
  )
where

import qualified Data.List as L
import Lens.Micro.Platform (makeLenses)
import Snake.Geometry (V2, add)

-- | A data type representing a segment with a start and end point.
data Segment = Segment
  { -- | The starting point of the segment.
    _start :: !(V2 Float),
    -- | The ending point of the segment.
    _end :: !(V2 Float)
  }
  deriving (Eq, Show)

makeLenses ''Segment

-- | A list of segments.
newtype Segments = Segments {unSegments :: [Segment]}
  deriving (Eq, Show)

-- | Create a list of snake segments.
--
-- The list will contain at least 3 segments which are generated in the opposite
-- direction of the velocity vector.
mkSegments ::
  -- | The number of additional segments to generate.
  Int ->
  -- | The velocity vector.
  V2 Float ->
  -- | The snake head position.
  V2 Float ->
  -- | A `Segments` list.
  Segments
mkSegments q v p =
  -- this is safe since the list always contains at least 3 segments
  let a : as = L.scanl' add p $ replicate (3 + abs q) ((*) (-1) <$> v)
   in Segments $ shift a (dup <$> as)
  where
    dup a = Segment a a

-- | Convert a `Segments` data structure to a list of `Segment`.
toList ::
  -- | The `Segments` data structure.
  Segments ->
  -- | A list of `Segment`.
  [Segment]
toList = unSegments

-- | Move the segments to a new position, maintaining the relative structure.
moveTo ::
  -- | The target snake head position.
  V2 Float ->
  -- | A `Segments` list.
  Segments ->
  -- | A `Segments` list with updated positions.
  Segments
moveTo p = Segments . shift p . unSegments

-- | Extend the segments by prepending a new segment.
extendTo ::
  -- | The target snake head position.
  V2 Float ->
  -- | A `Segments` list.
  Segments ->
  -- | An extended `Segments` list with updated positions.
  Segments
-- this is safe since the list always contains at least 3 segments
extendTo e' (Segments sss@((Segment _ e) : _)) = Segments $ Segment e e' : sss

-- | Get the current position of the snake head.
position ::
  -- | A `Segments` list.
  Segments ->
  -- | The current position of the snake head.
  V2 Float
-- this is safe since the list always contains at least 3 segments
position (Segments (Segment _ e : _)) = e

-- | Check if a point is contained within the `Segments` list.
containsPoint ::
  -- | The point to check.
  V2 Float ->
  -- | A `Segments` list.
  Segments ->
  -- | `True` if the point is contained within the segments, `False` otherwise.
  Bool
containsPoint p = elem p . fmap _end . unSegments

-- | Shift the segments based on a new snake head position.
shift ::
  -- | The target snake head position.
  V2 Float ->
  -- | A `Segments` list.
  [Segment] ->
  -- | A `Segments` list with updated positions.
  [Segment]
shift p ss = snd $ L.mapAccumL go p ss
  where
    go e' (Segment _ e) = (e, Segment e e')
