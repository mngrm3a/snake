module Snake.World.SegmentsSpec (spec) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Snake.Geometry.V2 (V2 (V2))
import Snake.World.Segments
  ( Segment (..),
    containsPoint,
    extendTo,
    mkSegments,
    moveTo,
    position,
    toList,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "mkSegments" $ do
    it "creates segments with the correct initial positions (+x)" $ do
      let xVelocity = V2 1 0
          segments = mkSegments 0 xVelocity $ V2 10 5
      toList segments `shouldBe` [seg 9 5 10 5, seg 8 5 9 5, seg 7 5 8 5]

    it "creates segments with the correct initial positions (-x)" $ do
      let _xVelocity = V2 (-1) 0
          segments = mkSegments 0 _xVelocity $ V2 10 5
      toList segments
        `shouldBe` [seg 11 5 10 5, seg 12 5 11 5, seg 13 5 12 5]

    it "creates segments with the correct initial positions (+y)" $ do
      let yVelocity = V2 0 1
          segments = mkSegments 0 yVelocity $ V2 10 5
      toList segments
        `shouldBe` [seg 10 4 10 5, seg 10 3 10 4, seg 10 2 10 3]

    it "creates segments with the correct initial positions (-y)" $ do
      let _yVelocity = V2 0 (-1)
          segments = mkSegments 0 _yVelocity $ V2 10 5
      toList segments
        `shouldBe` [seg 10 6 10 5, seg 10 7 10 6, seg 10 8 10 7]

    it "creates segments with the correct quantity and initial positions " $ do
      let quantity = 2
          segments = mkSegments quantity velocity $ V2 10 5
      toList segments
        `shouldBe` [ seg 9 5 10 5,
                     seg 8 5 9 5,
                     seg 7 5 8 5,
                     seg 6 5 7 5,
                     seg 5 5 6 5
                   ]

  describe "moveTo" $ do
    it "moves segments to the new position correctly shifting start and end positions" $ do
      let startPos = V2 10 5
          toPos = V2 11 5
          segments = moveTo toPos $ mkSegments 0 velocity startPos
      toList segments `shouldBe` [seg 10 5 11 5, seg 9 5 10 5, seg 8 5 9 5]

  describe "extendTo" $ do
    it "extends segments to the new position correctly shifting start and end positions" $ do
      let startPos = V2 10 5
          toPos = V2 11 5
          segments = extendTo toPos $ mkSegments 0 velocity startPos
      toList segments
        `shouldBe` [ seg 10 5 11 5,
                     seg 9 5 10 5,
                     seg 8 5 9 5,
                     seg 7 5 8 5
                   ]

  describe "position" $ do
    it "returns the position of the snake head" $ do
      let segments = mkSegments 0 velocity $ V2 10 5
      position segments `shouldBe` V2 10 5

  describe "containsPoint" $ do
    it "is true for all points of the snake" $ do
      let segments = mkSegments 0 velocity $ V2 10 5
          ends = segments & toList <&> _end
      all (`containsPoint` segments) ends `shouldBe` True

    it "is false for all other points" $ do
      let segments1 = mkSegments 0 velocity $ V2 10 5
          segments2 = mkSegments 0 velocity $ V2 10 10
          ends2 = segments2 & toList <&> _end
      any (`containsPoint` segments1) ends2 `shouldBe` False

seg :: Float -> Float -> Float -> Float -> Segment
seg x1 y1 x2 y2 = Segment (V2 x1 y1) (V2 x2 y2)

velocity :: V2 Float
velocity = V2 1 0
