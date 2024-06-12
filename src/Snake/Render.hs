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
  ( State (Collision, GetReady, Playing),
    World,
    game,
    info,
    lives,
    segmentSize,
    state,
    window,
  )

renderWorld :: World -> Gloss.Picture
renderWorld w =
  renderBackground w
    <> case w ^. state of
      GetReady -> renderGetReadyState w
      Playing -> renderPlayingState w
      Collision -> renderCollisionState w

renderBackground :: World -> Gloss.Picture
renderBackground w =
  renderGrid gridColor (w ^. segmentSize) (w ^. game)
    <> renderText Gloss.red (w ^. info & bottomLeft) 0.4 (show (w ^. lives))
  where
    gridColor = Gloss.greyN 0.9

renderGetReadyState :: World -> Gloss.Picture
renderGetReadyState w =
  renderSegmentsAt 0 w
    <> renderOverlay overlayColor (w ^. window)
  where
    overlayColor = Gloss.greyN 0.8 & Gloss.withAlpha 0.65

renderPlayingState :: World -> Gloss.Picture
renderPlayingState = renderSegmentsInterpolated

renderCollisionState :: World -> Gloss.Picture
renderCollisionState w =
  renderSegmentsAt 1 w
    <> renderOverlay overlayColor (w ^. game)
  where
    overlayColor = Gloss.red & Gloss.withAlpha 0.1
