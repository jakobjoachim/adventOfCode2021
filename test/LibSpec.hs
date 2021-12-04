module LibSpec (spec) where

import Test.Hspec

import Lib

spec :: Spec
spec = do
         describe "rotate" $ do
           it "should rotate complex list" $ do
            rotate [[1,2],[3,4]] `shouldBe` [[1,3],[2,4]]
       