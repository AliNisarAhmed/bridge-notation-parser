module Main where

import RIO
import Test.Hspec
import RBN
import qualified Text.Megaparsec as MP
import Data.Time

main :: IO ()
main = hspec do
  describe "Test Date Parser" do
    it "should correctly parse YYYYMMDD" do
      testYYYYMMDD
    it "should correctly parse YYYYMM" do
      testYYYYMM
    it "should correctly parse YYYY" do
      testYYYY
  describe "Test Article Info Parser" do
    it "should successfully parse both article title and author" do
      testBothTitleAndAuthor
    it "should successfully parse only author" do
      testAuthorOnly

testAuthorOnly :: IO ()
testAuthorOnly = do
  let input = "T :Ali Ahmed\n"
  let Right (ArticleInfo title author) = MP.parse articleInfoParser "" input
  title `shouldSatisfy` isNothing
  author `shouldBe` "Ali Ahmed"

testBothTitleAndAuthor :: IO ()
testBothTitleAndAuthor = do
  let input = "T This is my world:Ali Ahmed\n"
  let Right (ArticleInfo title author) = MP.parse articleInfoParser "" input
  title `shouldSatisfy` isJust
  title `shouldBe` (Just "This is my world")
  author `shouldBe` "Ali Ahmed"

testYYYYMMDD :: IO ()
testYYYYMMDD = do
  let date = "D 20201112\n"
  let Right maybeDay = MP.parse dateParser "" date
  maybeDay `shouldSatisfy` isJust
  let Just result = maybeDay
  let (year, monthOfYear, dayOfMonth) = toGregorian result
  year `shouldBe` 2020
  monthOfYear `shouldBe` 11
  dayOfMonth `shouldBe` 12

testYYYYMM :: IO ()
testYYYYMM = do
  let date = "D 202011\n"
  let Right maybeDay = MP.parse dateParser "" date
  maybeDay `shouldSatisfy` isJust
  let Just result = maybeDay
  let (y, m, d) = toGregorian result
  y `shouldBe` 2020
  m `shouldBe` 11
  d `shouldBe` 1

testYYYY :: IO ()
testYYYY = do
  let date = "D 2021\n"
  let Right maybeDay = MP.parse dateParser "" date
  maybeDay `shouldSatisfy` isJust
  let Just result = maybeDay
  let (y, m, d) = toGregorian result
  y `shouldBe` 2021
  m `shouldBe` 1
  d `shouldBe` 1