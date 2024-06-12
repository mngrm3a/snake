{-# LANGUAGE TemplateHaskell #-}

module Snake.World
  ( World,
    State (..),
    mkWorld,
    mkDefaultSegmentsAndVelocity,
    segmentSize,
    window,
    game,
    info,
    state,
    lives,
    clock,
    segments,
    velocity,
    keys,
  )
where

import Data.Sequence (Seq)
import Gloss.Extra.Clock (Clock, mkClock)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform (makeLenses)
import Snake.Geometry.Box (Box, boxOfSizeAt, boxOfSizeAtOrigin)
import Snake.Geometry.V2 (V2 (V2), V2F)
import Snake.World.Segments (Segments, mkSegments)

data World = World
  { -- environment
    _segmentSize :: !Float,
    _window :: !(Box Float),
    _game :: !(Box Float),
    _info :: !(Box Float),
    -- state
    _state :: !State,
    _lives :: !Int,
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
      _game = boxOfSizeAt gameWidth gameHeight gameCenter,
      _info =
        boxOfSizeAt gameWidth (2 * segmentSize') $
          V2 gcx (gcy + 0.5 * gameHeight + 1.5 * segmentSize'),
      -- state
      _state = GetReady,
      _lives = 3,
      _clock = mkClock $ 1 / 8,
      _segments = segments',
      _velocity = velocity',
      _keys = mempty
    }
  where
    segmentSize' = fromIntegral segmentSize''
    (winWidth, winHeight) = (fromIntegral winWidth', fromIntegral winHeight')
    (gameWidth, gameHeight) = (winWidth - 2 * segmentSize', winHeight - 4 * segmentSize')
    gameCenter@(V2 gcx gcy) = V2 0 $ (-1) * segmentSize'
    (segments', velocity') = mkDefaultSegmentsAndVelocity segmentSize'

mkDefaultSegmentsAndVelocity :: Float -> (Segments, V2F)
mkDefaultSegmentsAndVelocity segSize =
  ( mkSegments defaultNumExtraSegments defaultVelocity defaultPosition,
    defaultVelocity
  )
  where
    defaultNumExtraSegments = 17
    defaultVelocity = V2 segSize 0
    defaultPosition = V2 (segSize / 2) (segSize / 2)