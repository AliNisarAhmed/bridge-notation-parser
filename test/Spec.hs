module Main where

import Data.Time
import RBN
import RIO
import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as MP

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
  -- it "should successfully parse title that contains an escaped ':'"
  --   testTitleWithColon
  describe "Test Location Parser" do
    it "should successfully parse generic location only" do
      testGenericLocation
    it "shoudl successfully parse both generic and specific location" do
      testFullLocation
  describe "Test Event Parser" do
    it "should successfully parse generic Event only" do
      testGenericEvent
    it "should successfully parse both generic and specific event" do
      testFullEvent
  describe "Test Session Parser" do
    it "should successfully parse Segment" do
      testSegmentParser
    it "should successfully parse Knockout rounds (Quarters, Semis, Finals, Playoffs, Qualifying)" do
      testKnockoutRoundParser
    it "should successfully parse Round number" do
      testRoundParser
    it "should successfully parse Generic Round and Session" do
      testGenericSessionParser
  describe "Test Scoring Parser" do
    it "should successfully parse scoring without details" do
      testScoringWithoutDetailParser
    it "should successfully parse scoring with details" do
      testScoringWithDetailParser
  describe "Test Team Names parser" do
    it "should successfully parse team names without scores" do
      testTeamNamesWithoutScoresParser
    it "should successfully parse team names with scores" do
      testTeamNamesWithScoresParser
  describe "Test Players Parser" do
    it "should successfully parse the four players" do
      testParseFourPlayers
    it "should successfully parse North player name only" do
      testParseNorthOnly
    it "parse player names with room info" do
      testParsePlayerWithRooms
  describe "Test Board Number Parser" do
    it "should successfully parse board number with or without session" do
      testBoardNumberParser
  describe "Test Hands Parser" do
    it "should parse full deck" do
      testHandsParser

testHandsParser :: IO ()
testHandsParser = do
  let i1 = "H W:873.A6.KT864.KQ8:96.T54.97.AJ9643:T542.K93.AQ53.52:\n"
      northHand =
        makeHand
          ['8', '7', '3']
          ['A', '6']
          ['K', 'T', '8', '6', '4']
          ['K', 'Q', '8']
      eastHand =
        makeHand
          ['9', '6']
          ['T', '5', '4']
          ['9', '7']
          ['A', 'J', '9', '6', '4', '3']
      southHand =
        makeHand
          ['T', '5', '4', '2']
          ['K', '9', '3']
          ['A', 'Q', '5', '3']
          ['5', '2']
      westHand = makeHand ['A', 'K', 'Q', 'J'] ['Q', 'J', '8', '7', '2'] ['J', '2'] ['T', '7']
      result = Deck northHand eastHand southHand westHand
  MP.parse handsParser "" i1 `shouldParse` result

testBoardNumberParser :: IO ()
testBoardNumberParser = do
  let i1 = "B 7\n"
      i2 = "B 15:C\n"
      i3 = "B 10:7X01\n"
      i4 = "B 9:97-7-542\n"
      i5 = "B xy\n"
  MP.parse boardNumberParser "" i1 `shouldParse` BoardNumber "7" Nothing
  MP.parse boardNumberParser "" i2 `shouldParse` BoardNumber "15" (Just "C")
  MP.parse boardNumberParser "" i3 `shouldParse` BoardNumber "10" (Just "7X01")
  MP.parse boardNumberParser "" i4 `shouldParse` BoardNumber "9" (Just "97-7-542")
  MP.parse boardNumberParser "" i5 `shouldParse` BoardNumber "xy" Nothing

testParsePlayerWithRooms :: IO ()
testParsePlayerWithRooms = do
  let i1 = "N :Jan+Joe:O\n"
      i2 = "N :+Norman Kay:6\n"
      i3 = "N +Roth:GIB 4.0\n"
      west = Just $ Player West "Jan"
      east = Just $ Player East "Joe"
      east2 = Just $ Player East "Norman Kay"
      south = Just $ Player South "Roth"
      west2 = Just $ Player West "GIB 4.0"
  MP.parse playersParser "" i1 `shouldParse` Players Nothing Nothing west east Nothing (Just OpenRoom) Nothing
  MP.parse playersParser "" i2 `shouldParse` Players Nothing Nothing Nothing east2 (Just 6) Nothing Nothing
  MP.parse playersParser "" i3 `shouldParse` Players Nothing south west2 Nothing Nothing Nothing Nothing

testParseNorthOnly :: IO ()
testParseNorthOnly = do
  let i1 = "N Soloway\n"
      p1 = Just $ Player North "Soloway"
  MP.parse playersParser "" i1 `shouldParse` Players p1 Nothing Nothing Nothing Nothing Nothing Nothing

testParseFourPlayers :: IO ()
testParseFourPlayers = do
  let i1 = "N Wolff+Hamman:Stansby+Martel\n"
      i2 = "N Wolff+Hamman:Stansby+Martel:O:US Open\n"
      i3 = "N Wolff+Hamman:Stansby+Martel:6:US National Trials\n"
      p1 = Just $ Player North "Wolff"
      p2 = Just $ Player South "Hamman"
      p3 = Just $ Player West "Stansby"
      p4 = Just $ Player East "Martel"
  MP.parse playersParser "" i1 `shouldParse` Players p1 p2 p3 p4 Nothing Nothing Nothing
  MP.parse playersParser "" i2 `shouldParse` Players p1 p2 p3 p4 Nothing (Just OpenRoom) (Just "US Open")
  MP.parse playersParser "" i3 `shouldParse` Players p1 p2 p3 p4 (Just 6) Nothing (Just "US National Trials")

testTeamNamesWithScoresParser :: IO ()
testTeamNamesWithScoresParser = do
  let i1 = "K Italy:USA1:999:22\n"
      i2 = "K Iceland:Bulgaria:76.33:91.50\n"
      i3 = "K France:Spain::-5\n"
  MP.parse teamsParser "" i1 `shouldParse` Teams ("Italy", "USA1") (Just (999, 22))
  MP.parse teamsParser "" i2 `shouldParse` Teams ("Iceland", "Bulgaria") (Just (76.33, 91.5))

-- MP.parse teamsParser "" i3 `shouldParse` Teams ("France", "Spain") (Just (0, -5))

testTeamNamesWithoutScoresParser :: IO ()
testTeamNamesWithoutScoresParser = do
  let i1 = "K Nickell:Schwartz\n"
      i2 = "K Meckstroth's Marauders:Rodwell's Rockets\n"
  MP.parse teamsParser "" i1 `shouldParse` Teams ("Nickell", "Schwartz") Nothing
  MP.parse teamsParser "" i2 `shouldParse` Teams ("Meckstroth's Marauders", "Rodwell's Rockets") Nothing

testScoringWithDetailParser :: IO ()
testScoringWithDetailParser = do
  let i1 = "F X:Butler\n"
      i2 = "F I:1952\n"
      i3 = "F M:old\n"
      i4 = "F C:NS 60\n"
      i5 = "F A:no honors or partscore carryover\n"
  MP.parse scoringParser "" i1 `shouldParse` IMPPairs (Just $ GeneralScoreDetail "Butler")
  MP.parse scoringParser "" i2 `shouldParse` IMPs (Just $ YearOfScoring 1952)
  MP.parse scoringParser "" i3 `shouldParse` Matchpoints (Just $ GeneralScoreDetail "old")
  MP.parse scoringParser "" i4 `shouldParse` Chicago (Just $ NorthSouthPartscore 60)
  MP.parse scoringParser "" i5 `shouldParse` Cavendish (Just $ GeneralScoreDetail "no honors or partscore carryover")

testScoringWithoutDetailParser :: IO ()
testScoringWithoutDetailParser = do
  let i1 = "F I\n"
      i2 = "F N\n"
      i3 = "F R\n"
      i4 = "F C\n"
  MP.parse scoringParser "" i1 `shouldParse` IMPs Nothing
  MP.parse scoringParser "" i2 `shouldParse` InstantMatchpoints Nothing
  MP.parse scoringParser "" i3 `shouldParse` RubberBridge Nothing
  MP.parse scoringParser "" i4 `shouldParse` Chicago Nothing

testGenericSessionParser :: IO ()
testGenericSessionParser = do
  let i = "S Slam Bidding:Blackwood\n"
  MP.parse sessionParser "" i `shouldParse` GeneralSession "Slam Bidding" "Blackwood"

testRoundParser :: IO ()
testRoundParser = do
  let i1 = "S R32\n"
      i2 = "S R64:3\n"
  MP.parse sessionParser "" i1 `shouldParse` RoundOfNumber 32 Nothing
  MP.parse sessionParser "" i2 `shouldParse` RoundOfNumber 64 (Just 3)

testKnockoutRoundParser :: IO ()
testKnockoutRoundParser = do
  let q = "S Q:8\n"
  MP.parse sessionParser "" q `shouldParse` Quarterfinal 8
  let s = "S S:4\n"
  MP.parse sessionParser "" s `shouldParse` Semifinal 4
  let f = "S F:12\n"
  MP.parse sessionParser "" f `shouldParse` Final 12
  let p = "S P:9\n"
  MP.parse sessionParser "" p `shouldParse` Playoff 9
  let qy = "S I:9\n"
  MP.parse sessionParser "" qy `shouldParse` Qualifying 9

testSegmentParser :: IO ()
testSegmentParser = do
  let input = "S 2\n"
  MP.parse sessionParser "" input `shouldParse` Segment 2

testGenericEvent :: IO ()
testGenericEvent = do
  let input = "E 1999 Grand National Teams\n"
  MP.parse eventParser "" input `shouldParse` Event "1999 Grand National Teams" Nothing

testFullEvent :: IO ()
testFullEvent = do
  let input = "E Southeastern Regional:Flight A Open Pairs\n"
  MP.parse eventParser "" input `shouldParse` Event "Southeastern Regional" (Just "Flight A Open Pairs")

testFullLocation :: IO ()
testFullLocation = do
  let input = "L Edmonton, AB, Canada:Monticello Apartments\n"
  MP.parse locationParser "" input `shouldParse` Location "Edmonton, AB, Canada" (Just "Monticello Apartments")

testGenericLocation :: IO ()
testGenericLocation = do
  let input = "L Toronto, ON, Canada\n"
  MP.parse locationParser "" input `shouldParse` Location "Toronto, ON, Canada" Nothing

testAuthorOnly :: IO ()
testAuthorOnly = do
  let input = "T :Ali Ahmed\n"
  MP.parse articleInfoParser "" input `shouldParse` ArticleInfo Nothing "Ali Ahmed"

testBothTitleAndAuthor :: IO ()
testBothTitleAndAuthor = do
  let input = "T This is my world:Ali Ahmed\n"
  MP.parse articleInfoParser "" input `shouldParse` ArticleInfo (Just "This is my world") "Ali Ahmed"

-- testTitleWithColon :: IO ()
-- testTitleWithColon = do
--   let input = "T this title ::has colons:Ali Ahmed"
--   let Right (ArticleInfo title author) = MP.parse articleInfoParser "" input
--   title `shouldSatisfy` isJust
--   title `shouldBe` (Just "this title ::has colons")
--   author `shouldBe` "Ali Ahmed"

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