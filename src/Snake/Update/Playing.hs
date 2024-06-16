module Snake.Update.Playing (updatePlayingState) where

import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (First (First, getFirst))
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Lens.Micro.Platform (each, (%~), (&), (+~), (-~), (.~), (^.), (^..))
import Snake.Geometry.Box (BoxF)
import qualified Snake.Geometry.Box as Box
import Snake.Geometry.V2 (PointF, V2 (V2), V2F, add)
import Snake.World
  ( Status (Collision),
    WorldEnv,
    WorldState,
    food,
    game,
    gridPoints,
    keys,
    lives,
    points,
    rng,
    segmentSize,
    segments,
    status,
    velocity,
  )
import Snake.World.Segments (Segments, end, extendTo, moveTo, position)
import qualified Snake.World.Segments as Seg
import System.Random (uniformR)

updatePlayingState :: WorldEnv -> WorldState -> WorldState
updatePlayingState we ws =
  let (newVelocity, newPosition) = calcNewVelocityAndPosition we ws
   in if isColliding (we ^. game) (ws ^. segments) newPosition
        then
          ws
            & lives -~ 1
            & status .~ Collision
        else moveSegments we (ws & velocity .~ newVelocity) newPosition

moveSegments :: WorldEnv -> WorldState -> PointF -> WorldState
moveSegments we ws newPosition =
  ( if canEatAt newPosition
      then
        ws
          & segments %~ extendTo newPosition
          & points +~ 10
          & food .~ Nothing
      else ws & segments %~ moveTo newPosition
  )
    & placeFood we
  where
    canEatAt = (==) (ws ^. food) . Just

placeFood :: WorldEnv -> WorldState -> WorldState
placeFood we ws =
  -- TODO: what if there are no more free positions?
  let (idx, gen) = uniformR (0, length freePositions - 1) $ ws ^. rng
   in if ws ^. food & isNothing
        then ws & rng .~ gen & food .~ Just (freePositions !! idx)
        else ws
  where
    freePositions = filter (not . (`elem` segmentPositions)) $ we ^. gridPoints
    segmentPositions = (ws ^. segments & Seg.toList) ^.. each . end

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