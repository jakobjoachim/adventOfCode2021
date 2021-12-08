module LibSpec (spec) where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "rotate" $ do
    it "should rotate complex list" $ do
      rotate [[1, 2], [3, 4]] `shouldBe` [[1, 3], [2, 4]]

  describe "intersect" $ do
    it "should calculate intersecting elements for empty list" $ do
      intersect [1, 2] [3, 4] `shouldBe` []
    it "should calculate intersecting elements for one match" $ do
      intersect [1, 2] [3, 1] `shouldBe` [1]
