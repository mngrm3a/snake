{-# LANGUAGE DeriveTraversable #-}

-- | Module providing operations for 2D boxes.
module Snake.Geometry.Box
  ( -- * 2D Box Type
    Box (..),
    BoxF,
    BoxD,

    -- * Lenses
    center,
    halfSize,

    -- * Constructors
    boxF,
    boxD,
    boxOfSizeAtOrigin,
    boxOfSizeAt,

    -- * Box Properties
    topLeft,
    topRight,
    bottomLeft,
    bottomRight,
    size,
    area,

    -- * Box Operations
    containsPoint,
    containsBox,
  )
where

import Snake.Geometry.V2 (V2 (..))
import qualified Snake.Geometry.V2 as V2

-- | 2D Box type parameterized by scalar type 's'.
-- * The first field is the center point of the box.
-- * The second field is the half size of the box.
data Box s = Box
  { -- | The center point of the box.
    _center :: !(V2 s),
    -- | The half size of the box.
    _halfSize :: !(V2 s)
  }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

-- | Type synonym for 'Box' with 'Float' components.
type BoxF = Box Float

-- | Type synonym for 'Box' with 'Double' components.
type BoxD = Box Double

-- | Lens for the center point of a box.
center :: (Functor f) => (V2 s -> f (V2 s)) -> Box s -> f (Box s)
center f (Box c hs) = fmap (`Box` hs) (f c)

-- | Lens for the half size of a box.
halfSize :: (Functor f) => (V2 s -> f (V2 s)) -> Box s -> f (Box s)
halfSize f (Box c hs) = fmap (Box c) (f hs)

-- | Create a 'BoxF' from center and half-size vectors.
boxF ::
  -- | Center point.
  V2 Float ->
  -- | Half size.
  V2 Float ->
  -- | Resulting box.
  Box Float
boxF = Box

-- | Create a 'BoxD' from center and half-size vectors.
boxD ::
  -- | Center point.
  V2 Double ->
  -- | Half size.
  V2 Double ->
  -- | Resulting box.
  Box Double
boxD = Box

-- | Create a box of given dimensions centered at the origin.
boxOfSizeAtOrigin ::
  (Fractional s) =>
  -- | Width of the box.
  s ->
  -- | Height of the box.
  s ->
  -- | Resulting box.
  Box s
boxOfSizeAtOrigin w h = boxOfSizeAt w h V2.zero

-- | Create a box of given dimensions centered at a specified position.
boxOfSizeAt ::
  (Fractional s) =>
  -- | Width of the box.
  s ->
  -- | Height of the box.
  s ->
  -- | Center point of the box.
  V2 s ->
  -- | Resulting box.
  Box s
boxOfSizeAt w h c = Box c (V2 (0.5 * w) (0.5 * h))

-- | Get the top left corner of the box.
topLeft :: (Num s) => Box s -> V2 s
topLeft (Box (V2 cx cy) (V2 hx hy)) = V2 (cx - hx) (cy + hy)

-- | Get the top right corner of the box.
topRight :: (Num s) => Box s -> V2 s
topRight (Box (V2 cx cy) (V2 hx hy)) = V2 (cx + hx) (cy + hy)

-- | Get the bottom left corner of the box.
bottomLeft :: (Num s) => Box s -> V2 s
bottomLeft (Box (V2 cx cy) (V2 hx hy)) = V2 (cx - hx) (cy - hy)

-- | Get the bottom right corner of the box.
bottomRight :: (Num s) => Box s -> V2 s
bottomRight (Box (V2 cx cy) (V2 hx hy)) = V2 (cx + hx) (cy - hy)

-- | Get the full size of the box.
size :: (Num s) => Box s -> V2 s
size (Box _ hs) = V2.scale 2 hs

-- | Calculate the area of the box.
area :: (Num s) => Box s -> s
area box = let V2 w h = size box in w * h

-- | Check if a point is inside the box.
containsPoint :: (Ord s, Num s) => Box s -> V2 s -> Bool
containsPoint (Box (V2 cx cy) (V2 hx hy)) (V2 px py) = checkX && checkY
  where
    checkX = cx - hx <= px && px <= cx + hx
    checkY = cy - hy <= py && py <= cy + hy

-- | Check if another box is entirely contained within this box.
containsBox :: (Ord s, Num s) => Box s -> Box s -> Bool
containsBox (Box (V2 cx1 cy1) (V2 hx1 hy1)) (Box (V2 cx2 cy2) (V2 hx2 hy2)) =
  checkBottom && checkLeft && checkTop && checkRight
  where
    checkLeft = cx1 - hx1 <= cx2 - hx2
    checkRight = cx1 + hx1 >= cx2 + hx2
    checkBottom = cy1 - hy1 <= cy2 - hy2
    checkTop = cy1 + hy1 >= cy2 + hy2
