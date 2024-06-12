module Snake.Update.Playing (updatePlayingState) where

import Data.Maybe (fromMaybe)
import Data.Monoid (First (First, getFirst))
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform ((%~), (&), (.~))
import Snake.Geometry (BoxF, PointF, V2 (V2), V2F, add)
import qualified Snake.Geometry.Box as Box
import Snake.World
  ( Segments,
    State (Collision),
    World (_clock, _keys, _segmentSize, _segments, _velocity, _window),
    isResetting,
    keys,
    moveTo,
    position,
    segments,
    state,
    velocity,
  )
import qualified Snake.World.Segments as Seg

updatePlayingState :: World -> World
updatePlayingState w =
  if w & _clock & isResetting
    then realUpdatePlayingState w
    else w

realUpdatePlayingState :: World -> World
realUpdatePlayingState w =
  let (newVelocity, newPosition) = calcNewVelocityAndPosition w
   in if isColliding boundingBox segments' newPosition
        then w & state .~ Collision
        else
          w
            & segments %~ moveTo newPosition
            & velocity .~ newVelocity
            & keys .~ mempty
  where
    boundingBox = w & _window
    segments' = w & _segments

calcNewVelocityAndPosition :: World -> (V2F, PointF)
calcNewVelocityAndPosition w =
  let newVelocity =
        foldMap (First . keyToNewVelocity curVelocity segSize) curKeys
          & getFirst
          & fromMaybe curVelocity
   in (newVelocity, w & _segments & position & add newVelocity)
  where
    curVelocity = w & _velocity
    curKeys = w & _keys
    segSize = w & _segmentSize

keyToNewVelocity :: V2F -> Float -> Gloss.Key -> Maybe V2F
keyToNewVelocity (V2 vx vy) segSize key =
  case key of
    (Gloss.SpecialKey specialKey) -> go specialKey
    _anyOtherKey -> Nothing
  where
    go k = case k of
      Gloss.KeyRight | vx >= 0 -> jV2 segSize 0
      Gloss.KeyLeft | vx <= 0 -> jV2 (-segSize) 0
      Gloss.KeyUp | vy >= 0 -> jV2 0 segSize
      Gloss.KeyDown | vy <= 0 -> jV2 0 (-segSize)
      _nonArrowOrInvalidKey -> Nothing
    jV2 x' = Just . V2 x'

isColliding :: BoxF -> Segments -> PointF -> Bool
isColliding boundingBox segments' pos =
  not (Box.containsPoint boundingBox pos) || Seg.containsPoint pos segments'