module Days.Day5Spec where

import Days.Day5
import Test.Hspec

spec :: Spec
spec = do
  describe "getCloser" $ do
    it "should getCloser small to big" $ do
      getCloser 3 5 `shouldBe` 4
    it "should getCloser big to small" $ do
      getCloser 5 3 `shouldBe` 4
    it "should getCloser big to small" $ do
      getCloser 5 5 `shouldBe` 5

  describe "nextCoord" $ do
    it "should get next" $ do
      nextCoord (0, 1) (8, 9) `shouldBe` (1, 2)

  describe "allCoords" $ do
    it "should return all" $ do
      allCoords [((0, 5), (0, 3)), ((1, 2), (3, 2))] `shouldBe` [(0, 3), (0, 4), (0, 5), (3, 2), (2, 2), (1, 2)]
