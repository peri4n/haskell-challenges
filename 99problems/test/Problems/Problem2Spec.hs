module Problems.Problem2Spec where

import Test.Hspec

absolute :: Int -> Int
absolute n = if n < 0 then negate n else n

spec :: Spec
spec = do
  describe "absolute" $ do
    it "returns the original number when given a positive input" $
      absolute 1 `shouldBe` 1

    it "returns a positive number when given a negative input" $
      absolute (-1) `shouldBe` 1

    it "returns zero when given zero" $
      absolute 0 `shouldBe` 0
