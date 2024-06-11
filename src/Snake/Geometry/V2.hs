{-# LANGUAGE DeriveTraversable #-}

-- | Module providing operations for 2D vectors.
module Snake.Geometry.V2
  ( -- * 2D Vector Type
    V2 (..),
    V2F,
    V2D,
    PointF,
    PointD,

    -- * Lenses
    u,
    x,
    v,
    y,

    -- * Constructors
    v2F,
    v2D,

    -- * Operations
    fromTuple,
    toTuple,
    zero,
    add,
    sub,
    scale,
    dot,
    cross,
    magnitude,
    areParallel,
    areOrthogonal,
  )
where

-- | 2D Vector type parameterized by scalar type 's'.
-- * The first field is the U component @s@
-- * The second field is the V component @s@
data V2 s = V2
  { -- | The first component of the vector (X coordinate).
    _u :: !s,
    -- | The second component of the vector (Y coordinate).
    _v :: !s
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Lens for the first component (U coordinate) of a vector.
u, x :: (Functor f) => (s -> f s) -> V2 s -> f (V2 s)
u k vec = fmap (\u' -> vec {_u = u'}) (k (_u vec))
x = u

-- | Lens for the second component (V coordinate) of a vector.
v, y :: (Functor f) => (s -> f s) -> V2 s -> f (V2 s)
v k vec = fmap (\v' -> vec {_v = v'}) (k (_v vec))
y = v

-- | Type synonym for 'V2' with 'Float' components.
type V2F = V2 Float

-- | Type synonym for 'V2' with 'Float' components representing a point.
type PointF = V2 Float

-- | Type synonym for 'V2' with 'Double' components.
type V2D = V2 Double

-- | Type synonym for 'V2' with 'Double' components representing a point.
type PointD = V2 Double

-- | Create a 'V2F' from two 'Float' values.
v2F :: Float -> Float -> V2 Float
v2F = V2

-- | Create a 'V2D' from two 'Double' values.
v2D :: Double -> Double -> V2 Double
v2D = V2

-- | Create a 'V2' from a tuple.
fromTuple ::
  -- | Tuple representing U and V components.
  (s, s) ->
  -- | Resulting 2D vector.
  V2 s
fromTuple (u', v') = V2 u' v'

-- | Convert a 'V2' to a tuple.
toTuple ::
  -- | Input 2D vector.
  V2 s ->
  -- | Resulting tuple representing U and V components.
  (s, s)
toTuple (V2 u' v') = (u', v')

-- | Create a zero vector.
zero :: (Num s) => V2 s
zero = V2 0 0

-- | Add two vectors.
add :: (Num s) => V2 s -> V2 s -> V2 s
add (V2 u1 v1) (V2 u2 v2) = V2 (u1 + u2) (v1 + v2)

-- | Subtract one vector from another.
sub :: (Num s) => V2 s -> V2 s -> V2 s
sub (V2 u1 v1) (V2 u2 v2) = V2 (u1 - u2) (v1 - v2)

-- | Scale a vector by a scalar value.
scale :: (Num s) => s -> V2 s -> V2 s
scale f (V2 u' v') = V2 (f * u') (f * v')

-- | Compute the dot product of two vectors.
dot :: (Num s) => V2 s -> V2 s -> s
dot (V2 u1 v1) (V2 u2 v2) = u1 * u2 + v1 * v2

-- | Compute the cross product of two vectors.
cross :: (Num s) => V2 s -> V2 s -> s
cross (V2 u1 v1) (V2 u2 v2) = u1 * v2 - u2 * v1

-- | Compute the magnitude (length) of a vector.
magnitude :: (Floating s) => V2 s -> s
magnitude (V2 u' v') = sqrt (u' * u' + v' * v')

-- | Check if two vectors are parallel.
areParallel :: (Eq s, Floating s) => V2 s -> V2 s -> Bool
areParallel vec1 vec2 = cross vec1 vec2 == 0

-- | Check if two vectors are orthogonal (perpendicular).
areOrthogonal :: (Eq s, Num s) => V2 s -> V2 s -> Bool
areOrthogonal vec1 vec2 = dot vec1 vec2 == 0
