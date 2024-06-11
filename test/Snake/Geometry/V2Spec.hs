module Snake.Geometry.V2Spec (spec) where

import Snake.Geometry.V2 (v2F)
import Snake.Geometry.V2 as V2
  ( V2 (..),
    add,
    areOrthogonal,
    areParallel,
    cross,
    dot,
    fromTuple,
    magnitude,
    scale,
    sub,
    toTuple,
    v2D,
    zero,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "v2F" $ do
    it "creates a V2F vector" $ do
      let V2 x y = v2F 1 2
      x `shouldBe` 1
      y `shouldBe` 2

  describe "v2D" $ do
    it "creates a V2D vector" $ do
      let V2 x y = v2D 1 2
      x `shouldBe` 1
      y `shouldBe` 2

  describe "fromTuple" $ do
    it "creates a vector from a tuple" $ do
      let V2 x y = V2.fromTuple (1, 2) :: V2 Float
      x `shouldBe` 1
      y `shouldBe` 2

  describe "toTuple" $ do
    it "converts a vector to a tuple" $ do
      V2.toTuple (v2F 1 2) `shouldBe` (1, 2)

  describe "zero" $ do
    it "creates a zero vector" $ do
      let V2 x y = V2.zero :: V2 Float
      x `shouldBe` 0
      y `shouldBe` 0

  describe "add" $ do
    it "adds two vectors" $ do
      V2.add (v2F 1 2) (v2F 3 4) `shouldBe` v2F 4 6

  describe "sub" $ do
    it "subtracts one vector from another" $ do
      V2.sub (v2F 3 4) (v2F 1 2) `shouldBe` v2F 2 2

  describe "scale" $ do
    it "scales a vector by a scalar" $ do
      V2.scale 2 (v2F 1 2) `shouldBe` v2F 2 4

  describe "dot" $ do
    it "computes the dot product of two vectors" $ do
      V2.dot (v2F 1 2) (v2F 3 4) `shouldBe` 11

  describe "cross" $ do
    it "computes the cross product of two vectors" $ do
      V2.cross (v2F 1 2) (v2F 3 4) `shouldBe` -2

  describe "magnitude" $ do
    it "computes the magnitude of a vector" $ do
      V2.magnitude (v2F 3 4) `shouldBe` 5

  describe "areParallel" $ do
    it "returns True if vectors are parallel" $ do
      V2.areParallel (v2F 1 2) (v2F 2 4) `shouldBe` True

    it "returns False if vectors are not parallel" $ do
      V2.areParallel (v2F 1 2) (v2F 2 3) `shouldBe` False

  describe "areOrthogonal" $ do
    it "returns True if vectors are orthogonal" $ do
      V2.areOrthogonal (v2F 1 0) (v2F 0 1) `shouldBe` True

    it "returns False if vectors are not orthogonal" $ do
      V2.areOrthogonal (v2F 1 2) (v2F 2 3) `shouldBe` False
