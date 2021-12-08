module Days.Day8Spec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Days.Day8 (containsChars)
import qualified Days.Day8 as SUT
import Lib (findFirst, intersect)
import Test.Hspec

spec :: Spec
spec = do
  let xs = ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"]
  let ys = Map.fromList [(Set.fromList"agebfd", '0'), (Set.fromList"be", '1'), (Set.fromList"cbdgef", '9'), (Set.fromList"cfbegad", '8'), (Set.fromList"cgeb", '4'), (Set.fromList"edb", '7'), (Set.fromList"fabcd", '2'), (Set.fromList"fdcge", '5'), (Set.fromList"fecdb", '3'), (Set.fromList"fgaecd", '6')]
  describe "findNumber" $ do
    it "one" $ do
      findFirst (\x -> length x == 2) xs `shouldBe` "be"
    it "four" $ do
      findFirst (\x -> length x == 4) xs `shouldBe` "cgeb"
    it "seven" $ do
      findFirst (\x -> length x == 3) xs `shouldBe` "edb"
    it "eight" $ do
      findFirst (\x -> length x == 7) xs `shouldBe` "cfbegad"
    it "six" $ do
      findFirst (\x -> length x == 6 && not (x `containsChars` "be")) xs `shouldBe` "fgaecd"
    it "two" $ do
      findFirst (\x -> length x == 5 && not (x `containsChars` [head $ intersect "fgaecd" "be"])) xs `shouldBe` "fabcd"
    it "three" $ do
      findFirst (\x -> length x == 5 && x `containsChars` "be" && x /= "fabcd") xs `shouldBe` "fecdb"
    it "five" $ do
      findFirst (\x -> length x == 5 && x /= "fabcd" && x /= "fecdb") xs `shouldBe` "fdcge"
    it "nine" $ do
      findFirst (\x -> length x == 6 && x `containsChars` "be" && x `containsChars` "fdcge") xs `shouldBe` "cbdgef"
    it "zero" $ do
      findFirst (\x -> length x == 6 && x /= "cbdgef" && x /= "fgaecd") xs `shouldBe` "agebfd"

  describe "findNumbers" $ do
    it "should create Map" $ do
      SUT.findNumbers xs `shouldBe` ys

  describe "applyNumberMap" $ do
    it "should apply map" $ do
      SUT.applyNumberMap ys [Set.fromList "fdgacbe", Set.fromList "cefdb", Set.fromList "cefbgd", Set.fromList "gcbe"] `shouldBe` 8394
