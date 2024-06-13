module Snake.Update.Playing (updatePlayingState) where

import Data.Maybe (fromMaybe)
import Data.Monoid (First (First, getFirst))
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform ((%~), (&), (-~), (.~), (^.))
import Snake.Geometry.Box (BoxF)
import qualified Snake.Geometry.Box as Box
import Snake.Geometry.V2 (PointF, V2 (V2), V2F, add)
import Snake.World
  ( Status (Collision),
    WorldEnv,
    WorldState,
    game,
    keys,
    lives,
    segmentSize,
    segments,
    status,
    velocity,
  )
import Snake.World.Segments (Segments, moveTo, position)
import qualified Snake.World.Segments as Seg

updatePlayingState :: WorldEnv -> WorldState -> WorldState
updatePlayingState we ws =
  let (newVelocity, newPosition) = calcNewVelocityAndPosition we ws
   in if isColliding (we ^. game) (ws ^. segments) newPosition
        then
          ws
            & lives -~ 1
            & status .~ Collision
        else
          ws
            & segments %~ moveTo newPosition
            & velocity .~ newVelocity

calcNewVelocityAndPosition :: WorldEnv -> WorldState -> (V2F, PointF)
calcNewVelocityAndPosition we ws =
  let newVelocity =
        foldMap (First . keyToNewVelocity curVelocity segSize) curKeys
          & getFirst
          & fromMaybe curVelocity
   in (newVelocity, ws ^. segments & position & add newVelocity)
  where
    curVelocity = ws ^. velocity
    curKeys = ws ^. keys
    segSize = we ^. segmentSize

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