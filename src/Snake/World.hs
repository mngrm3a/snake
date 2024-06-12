{-# LANGUAGE TemplateHaskell #-}

module Snake.World
  ( module Gloss.Extra.Clock,
    module Snake.World.Segments,
    World (..),
    State (..),
    mkWorld,
    segmentSize,
    window,
    state,
    clock,
    segments,
    velocity,
    keys,
  )
where

import Data.Sequence (Seq)
import Gloss.Extra.Clock
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform (makeLenses)
import Snake.Geometry.Box (Box, boxOfSizeAtOrigin)
import Snake.Geometry.V2 (V2 (V2))
import Snake.World.Segments

data World = World
  { -- environment
    _segmentSize :: !Float,
    _window :: !(Box Float),
    -- state
    _state :: !State,
    _clock :: !Clock,
    _segments :: !Segments,
    _velocity :: !(V2 Float),
    _keys :: !(Seq Gloss.Key)
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
      _velocity = rightwardsVelocity,
      _keys = mempty
    }
  where
    (winWidth, winHeight) = (fromIntegral winWidth', fromIntegral winHeight')
    segmentSize' = fromIntegral segmentSize''
    rightwardsVelocity = V2 segmentSize' 0
    initialPosition = V2 (segmentSize' / 2) (segmentSize' / 2)
