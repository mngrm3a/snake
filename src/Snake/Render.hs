module Snake.Render (renderWorld) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&), (^.))
import Snake.Geometry.Box (bottomLeft, bottomRight, center, halfSize)
import Snake.Geometry.V2 (V2 (..), add, sub)
import Snake.Render.Segments
  ( renderSegmentsAt,
    renderSegmentsInterpolated,
  )
import Snake.Render.Utils (renderCell, renderGrid, renderOverlay, renderText)
import Snake.World

renderWorld :: World -> Gloss.Picture
renderWorld w =
  renderBackground we ws
    <> case w ^. state . status of
      GetReady -> renderGetReadyState we ws
      Playing -> renderPlayingState we ws
      Collision -> renderCollisionState we ws
  where
    we = w ^. env
    ws = w ^. state

renderBackground :: WorldEnv -> WorldState -> Gloss.Picture
renderBackground we ws =
  renderGrid gridColor (we ^. segmentSize) (we ^. game)
    <> renderText livesColor (we ^. info & bottomLeft) 0.4 (show (ws ^. lives))
    <> renderText pointsColor pointsPos 0.4 (show $ ws ^. points)
    <> maybe mempty (\p -> renderCell foodColor p (we ^. segmentSize)) (ws ^. food)
  where
    pointsPos = sub (we ^. info . center) $ V2 (we ^. segmentSize) (we ^. segmentSize)
    pointsColor = Gloss.azure
    gridColor = Gloss.greyN 0.9
    foodColor = Gloss.orange & Gloss.light
    livesColor = case ws ^. lives of
      3 -> Gloss.green
      _ -> Gloss.red

renderGetReadyState :: WorldEnv -> WorldState -> Gloss.Picture
renderGetReadyState we ws =
  renderSegmentsAt we ws 0
    <> renderOverlay overlayColor (we ^. window)
  where
    overlayColor = Gloss.greyN 0.8 & Gloss.withAlpha 0.65

renderPlayingState :: WorldEnv -> WorldState -> Gloss.Picture
renderPlayingState = renderSegmentsInterpolated

renderCollisionState :: WorldEnv -> WorldState -> Gloss.Picture
renderCollisionState we ws =
  renderSegmentsAt we ws 1
    <> renderOverlay overlayColor (we ^. game)
  where
    overlayColor = Gloss.red & Gloss.withAlpha 0.1
