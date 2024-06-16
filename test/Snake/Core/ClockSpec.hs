module Snake.Core.ClockSpec (spec) where

import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import Snake.Core.Clock
  ( Clock,
    elapsed,
    mkClock,
    progress,
    tick,
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )

spec :: Spec
spec = do
  describe "tick" $ do
    it "resets a new clock" $ do
      let (resets, clock) = clock5 & 1 `ticksOf` 1
      resets `shouldBe` [True]
      elapsed clock `shouldBe` 1
      progress clock `shouldBe` 1 / 5

    it "resets after a full period has elapsed" $ do
      let (resets, clock) = clock5 & 5 `ticksOf` 1
      resets `shouldBe` [True, False, False, False, True]
      elapsed clock `shouldBe` 0
      progress clock `shouldBe` 1

    it "resets after more than a full period has elapsed" $ do
      let (resets, clock) = clock5 & 5 `ticksOf` 1.1
      resets `shouldBe` [True, False, False, False, True]
      elapsed clock `shouldSatisfy` (> 0.499999)
      progress clock `shouldSatisfy` (> 0.099999)

clock5 :: Clock
clock5 = mkClock 5

infix 3 `ticksOf`

ticksOf :: Int -> Float -> Clock -> ([Bool], Clock)
ticksOf n dt clock = first reverse $ (!! n) $ iterate go ([], clock)
  where
    go (rs, c) = let (r', c') = tick dt c in (r' : rs, c')