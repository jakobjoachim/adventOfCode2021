module Days.Day4Spec (spec) where

import Days.Day4
import Test.Hspec

spec :: Spec
spec = do
  describe "winningBoardIndex" $ do
    it "should find winning column" $ do
      winningBoardIndex [[[1, 2], [3, 4]], [[1, -1], [2, -1]]] `shouldBe` Just 1

    it "should find winning row" $ do
      winningBoardIndex [[[-1, -1], [3, 4]], [[1, 3], [2, 5]]] `shouldBe` Just 0

  describe "replaceDrawnNumber" $ do
    it "should replace all 3s with -1" $ do
      replaceDrawnNumber [[[1, 2], [3, 4]], [[1, 3], [3, 4]]] 3 `shouldBe` [[[1, 2], [-1, 4]], [[1, -1], [-1, 4]]]
