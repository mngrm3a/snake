{-# LANGUAGE TemplateHaskell #-}

module Snake.World
  ( World,
    WorldEnv,
    WorldState,
    Status (..),
    mkWorld,
    mkWorldStateFromWorldEnv,
    env,
    state,
    segmentSize,
    window,
    game,
    info,
    status,
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
import Lens.Micro.Platform (makeLenses, (&))
import Snake.Geometry.Box (Box, boxOfSizeAt, boxOfSizeAtOrigin)
import Snake.Geometry.V2 (V2 (V2), scale)
import Snake.World.Segments (Segments, mkSegments)

data World = World
  { _env :: !WorldEnv,
    _state :: !WorldState
  }
  deriving (Eq, Show)

data WorldEnv = WorldEnv
  { _segmentSize :: !Float,
    _window :: !(Box Float),
    _game :: !(Box Float),
    _info :: !(Box Float)
  }
  deriving (Eq, Show)

data WorldState = WorldState
  { _status :: !Status,
    _lives :: !Int,
    _clock :: !Clock,
    _segments :: !Segments,
    _velocity :: !(V2 Float),
    _keys :: !(Seq Gloss.Key)
  }
  deriving (Eq, Show)

data Status = GetReady | Playing | Collision deriving (Eq, Show)

makeLenses ''World
makeLenses ''WorldEnv
makeLenses ''WorldState

mkWorld :: (Int, Int) -> Float -> World
mkWorld winSize segSize =
  World
    { _env = mkWorldEnv winSize segSize,
      _state = mkWorldState segSize
    }

mkWorldEnv :: (Int, Int) -> Float -> WorldEnv
mkWorldEnv (winWidth', winHeight') segSize =
  WorldEnv
    { _segmentSize = segSize,
      _window = boxOfSizeAtOrigin winWidth winHeight,
      _game = boxOfSizeAt gameWidth gameHeight gameCenter,
      _info =
        boxOfSizeAt gameWidth (2 * segSize) $
          V2 gcx (gcy + 0.5 * gameHeight + 1.5 * segSize)
    }
  where
    (winWidth, winHeight) = (fromIntegral winWidth', fromIntegral winHeight')
    (gameWidth, gameHeight) = (winWidth - 2 * segSize, winHeight - 4 * segSize)
    gameCenter@(V2 gcx gcy) = V2 0 $ (-1) * segSize

mkWorldState :: Float -> WorldState
mkWorldState segSize =
  WorldState
    { _status = GetReady,
      _lives = 3,
      _clock = mkClock $ 1 / 8,
      _segments = mkSegments 17 defaultVelocity $ scale 0.5 $ V2 segSize segSize,
      _velocity = defaultVelocity,
      _keys = mempty
    }
  where
    defaultVelocity = V2 segSize 0

mkWorldStateFromWorldEnv :: WorldEnv -> WorldState
mkWorldStateFromWorldEnv we = mkWorldState (we & _segmentSize)