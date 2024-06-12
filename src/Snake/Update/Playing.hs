module Snake.Update.Playing (updatePlayingState) where

import Data.Maybe (fromMaybe)
import Data.Monoid (First (First, getFirst))
import Gloss.Extra.Clock (isResetting)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform ((%~), (&), (.~), (^.))
import Snake.Geometry.Box (BoxF)
import qualified Snake.Geometry.Box as Box
import Snake.Geometry.V2 (PointF, V2 (V2), V2F, add)
import Snake.World
  ( State (Collision),
    World,
    clock,
    keys,
    segmentSize,
    segments,
    state,
    velocity,
    window,
  )
import Snake.World.Segments (Segments, moveTo, position)
import qualified Snake.World.Segments as Seg

updatePlayingState :: World -> World
updatePlayingState w =
  if w ^. clock & isResetting
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
    boundingBox = w ^. window
    segments' = w ^. segments

calcNewVelocityAndPosition :: World -> (V2F, PointF)
calcNewVelocityAndPosition w =
  let newVelocity =
        foldMap (First . keyToNewVelocity curVelocity segSize) curKeys
          & getFirst
          & fromMaybe curVelocity
   in (newVelocity, w ^. segments & position & add newVelocity)
  where
    curVelocity = w ^. velocity
    curKeys = w ^. keys
    segSize = w ^. segmentSize

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