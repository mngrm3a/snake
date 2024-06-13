module Snake.Render (renderWorld) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&), (^.))
import Snake.Geometry.Box (bottomLeft)
import Snake.Render.Segments
  ( renderSegmentsAt,
    renderSegmentsInterpolated,
  )
import Snake.Render.Utils (renderGrid, renderOverlay, renderText)
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
    <> renderText Gloss.red (we ^. info & bottomLeft) 0.4 (show (ws ^. lives))
  where
    gridColor = Gloss.greyN 0.9

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
