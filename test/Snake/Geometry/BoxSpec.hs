module Snake.Geometry.BoxSpec (spec) where

import Data.Function ((&))
import Snake.Geometry.Box (Box (..))
import qualified Snake.Geometry.Box as Box
import Snake.Geometry.V2 (V2 (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "boxOfSizeAtOrigin" $
    it "creates a box of given size centered at the origin" $ do
      let Box givenCenter givenHalfSize = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      givenCenter `shouldBe` V2 0 0
      givenHalfSize `shouldBe` V2 2 1.5

  describe "boxOfSizeAt" $
    it "creates a box of given size centered at the given position" $ do
      let Box givenCenter givenHalfSize = Box.boxOfSizeAt 4 3 (V2 5 5) :: Box Float
      givenCenter `shouldBe` V2 5 5
      givenHalfSize `shouldBe` V2 2 1.5

  describe "topLeft" $
    it "computes the top left corner of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.topLeft box `shouldBe` V2 (-2) 1.5

  describe "topRight" $
    it "computes the top right corner of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.topRight box `shouldBe` V2 2 1.5

  describe "bottomLeft" $
    it "computes the bottom left corner of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.bottomLeft box `shouldBe` V2 (-2) (-1.5)

  describe "bottomRight" $
    it "computes the bottom right corner of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.bottomRight box `shouldBe` V2 2 (-1.5)

  describe "halfSize" $
    it "computes the half size of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      (box & Box._halfSize) `shouldBe` V2 2 1.5

  describe "center" $
    it "computes the position of the box" $ do
      let box = Box.boxOfSizeAt 4 3 (V2 5 5) :: Box Float
      (box & Box._center) `shouldBe` V2 5 5

  describe "size" $
    it "computes the size of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.size box `shouldBe` V2 4 3

  describe "area" $
    it "computes the area of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.area box `shouldBe` 12.0

  describe "containsPoint" $ do
    it "returns True if the point is inside the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.containsPoint box (V2 1 1) `shouldBe` True

    it "returns True if the point is on the edge of the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.containsPoint box (V2 2 1.5) `shouldBe` True

    it "returns False if the point is outside the box" $ do
      let box = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      Box.containsPoint box (V2 5 5) `shouldBe` False

  describe "containsBox" $ do
    it "returns True if the box is contained entirely within another box" $ do
      let box1 = Box.boxOfSizeAtOrigin 4 3 :: Box Float
      let box2 = Box.boxOfSizeAtOrigin 2 1.5 :: Box Float
      Box.containsBox box1 box2 `shouldBe` True

    it "returns False if the box is not contained entirely within another box" $ do
      let box1 = Box.boxOfSizeAt 4 3 (V2 1 1) :: Box Float
      let box2 = Box.boxOfSizeAt 5 4 (V2 1 1) :: Box Float
      Box.containsBox box1 box2 `shouldBe` False
