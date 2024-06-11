module Gloss.Extra.ClockSpec (spec) where

import Gloss.Extra.Clock
  ( Clock,
    isResetting,
    mkClock,
    progress,
    tick,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "isResetting" $ do
    it "returns True if clock is new" $
      isResetting clock5 `shouldBe` True

    it "returns True if clock is resetting" $
      isResetting (tick 5 clock5) `shouldBe` True

    it "returns False if clock is not resetting" $
      isResetting (tick 1 clock5) `shouldBe` False

  describe "progress" $ do
    it "returns the progress of the clock" $
      progress (tick 2.5 clock5) `shouldBe` 0.5

clock5 :: Clock
clock5 = mkClock 5