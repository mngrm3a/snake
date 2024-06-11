{-# LANGUAGE TemplateHaskell #-}

module Snake.World
  ( module Gloss.Extra.Clock,
    module Snake.World.Segments,
    World (..),
    mkWorld,
    segmentSize,
    window,
    state,
    clock,
    segments,
    velocity,
  )
where

import Gloss.Extra.Clock
import Lens.Micro.Platform (makeLenses)
import Snake.Geometry (Box, V2 (V2), boxOfSizeAtOrigin)
import Snake.World.Segments

data World = World
  { -- environment
    _segmentSize :: !Float,
    _window :: !(Box Float),
    -- state
    _state :: !State,
    _clock :: !Clock,
    _segments :: !Segments,
    _velocity :: !(V2 Float)
  }
  deriving (Eq, Show)

data State = GetReady | Playing | Collision deriving (Eq, Show)

makeLenses ''World

mkWorld :: (Int, Int) -> Int -> World
mkWorld (winWidth', winHeight') segmentSize'' =
  World
    { -- environment
      _segmentSize = segmentSize',
      _window = boxOfSizeAtOrigin winWidth winHeight,
      -- state
      _state = GetReady,
      _clock = mkClock $ 1 / 8,
      _segments = mkSegments 0 rightwardsVelocity initialPosition,
      _velocity = rightwardsVelocity
    }
  where
    (winWidth, winHeight) = (fromIntegral winWidth', fromIntegral winHeight')
    segmentSize' = fromIntegral segmentSize''
    rightwardsVelocity = V2 segmentSize' 0
    initialPosition = V2 (segmentSize' / 2) (segmentSize' / 2)
