module Snake.Render (renderWorld) where

import qualified Graphics.Gloss as Gloss
import Lens.Micro.Platform ((&), (^.))
import Snake.Render.Segments
  ( renderInterpolatedSegmentPositions,
    renderSegmentEndPositions,
  )
import Snake.Render.Utils (renderGrid, renderOverlay)
import Snake.World
  ( State (Collision, GetReady, Playing),
    World,
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
  renderGrid gridColor (w ^. segmentSize) (w ^. window)
  where
    gridColor = Gloss.greyN 0.9

renderGetReadyState :: World -> Gloss.Picture
renderGetReadyState w =
  renderSegmentEndPositions w
    <> renderOverlay overlayColor (w ^. window)
  where
    overlayColor = Gloss.greyN 0.8 & Gloss.withAlpha 0.65

renderPlayingState :: World -> Gloss.Picture
renderPlayingState = renderInterpolatedSegmentPositions

renderCollisionState :: World -> Gloss.Picture
renderCollisionState = const Gloss.blank
