module Main where

import Data.Time
import Domain.Types
import RBN
import RIO
import qualified RIO.Map as Map
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
    it "should parse full deck without fourth hand" do
      testHandsParser
    it "should parse full deck with voids" do
      testHandsWithVoids
    it "should successfully parse when only pair hands are given" do
      testParsePairHands
    it "should parse only one hand given" do
      testSingleHandDeals
    it "should successfully parse partial deals / end positions" do
      testPartialDeals
    it "should successfully parse partial single suit deals" do
      testSingleSuitPartialDeals
    it "should successfully parse hidden hands" do
      testParseHiddenHands
    it "should parse same deal correctly no matter starting player" do
      testParseSameDealDifferentOrientation
  describe "Test Auction Parser" do
    it "should parse auction that ends in all pass" do
      testAllPassAuctionParser
    it "should parse Partial Auctions" do
      testPartialAuctionParser
    it "should parse 'your call' embedded within auctions" do
      testYourCallParser
    it "should successfully parse annotations" do
      testAnnotationParser
    it "can parse notes in isolation" do
      testNotesParser
    it "should successfully parse notes given after the auction" do
      testAnnotationNotesParser
  describe "Test Contract Parser" do
    it "should parse undoubled contract and declarer" do
      testUndoubleContractAndDeclarerParser
    it "should correctly parse double and redoubled contracts" do
      testDoubledAndRedbledContracts
    it "should parse contracts given with a goal" do
      testContractWithGoalsParser
    it "should parse given on-lead player" do
      testContractWithOnLeadPlayer
    it "should correctly parse partial deal with trick goals" do
      testPartialContractWithTrickGoal
  describe "Test Play Parser" do
    it "should correctly parse tricks without annotations" do
      testParsePlayWithoutAnnotation
    it "should correctly parse tricks with annotations" do
      testParsePlayWithAnnotation
    it "should correctly parse tricks with notes given at the end" do
      testParsePlayWithNotes
    it "should correctly parse pseudo plays" do
      testParsePseudoPlays
  describe "Test Result Parser" do
    it "should parse correctly when only Number of tricks are given" do
      testNumberOfTricksOnlyResult

testNumberOfTricksOnlyResult :: IO ()
testNumberOfTricksOnlyResult = do
  let i1 = "R 11\n"
      result1 =
        DealResult (Just ElevenTricks) Nothing Nothing
  MP.parse resultParser "" i1 `shouldParse` result1

testParsePseudoPlays :: IO ()
testParsePseudoPlays = do
  let i1 = "P SK--.:STQA.\n"
      i2 = "P SK--.:STQA.:--+-:S578.\n"
      result1 =
        [ Trick
            (PlayedCard SuitLed Spades King Nothing)
            LowestUnplayed
            LowestUnplayed
            ImmaterialDiscard,
          Trick
            (PlayedCard SuitLed Spades Ten Nothing)
            (PlayedCard SuitFollowed Spades Queen Nothing)
            (PlayedCard SuitFollowed Spades Ace Nothing)
            ImmaterialDiscard
        ]
      result2 =
        result1
          ++ [ Trick
                 LowestUnplayed
                 LowestUnplayed
                 HighestUnplayed
                 LowestUnplayed,
               Trick
                 (PlayedCard SuitLed Spades Five Nothing)
                 (PlayedCard SuitFollowed Spades Seven Nothing)
                 (PlayedCard SuitFollowed Spades Eight Nothing)
                 ImmaterialDiscard
             ]
  MP.parse playParser "" i1 `shouldParse` result1
  MP.parse playParser "" i2 `shouldParse` result2

testParsePlayWithNotes :: IO ()
testParsePlayWithNotes = do
  let i1 = "P HQ*3J2:HK47A:DJA53:HT68C7^1\n1 subtle falsecard\n"
      result1 =
        [ Trick
            (PlayedCard SuitLed Hearts Queen (Just Conventional))
            (PlayedCard SuitFollowed Hearts Three Nothing)
            (PlayedCard SuitFollowed Hearts Jack Nothing)
            (PlayedCard SuitFollowed Hearts Two Nothing),
          Trick
            (PlayedCard SuitLed Hearts King Nothing)
            (PlayedCard SuitFollowed Hearts Four Nothing)
            (PlayedCard SuitFollowed Hearts Seven Nothing)
            (PlayedCard SuitFollowed Hearts Ace Nothing),
          Trick
            (PlayedCard SuitLed Diamonds Jack Nothing)
            (PlayedCard SuitFollowed Diamonds Ace Nothing)
            (PlayedCard SuitFollowed Diamonds Five Nothing)
            (PlayedCard SuitFollowed Diamonds Three Nothing),
          Trick
            (PlayedCard SuitLed Hearts Ten Nothing)
            (PlayedCard SuitFollowed Hearts Six Nothing)
            (PlayedCard SuitFollowed Hearts Eight Nothing)
            (PlayedCard Discard Clubs Seven (Just $ Note 1 "subtle falsecard"))
        ]
  MP.parse playParser "" i1 `shouldParse` result1

testParsePlayWithAnnotation :: IO ()
testParsePlayWithAnnotation = do
  let i1 = "P HQ*3J2:HK47A:DJA53:HT68C7^1\n"
      result1 =
        [ Trick
            (PlayedCard SuitLed Hearts Queen (Just Conventional))
            (PlayedCard SuitFollowed Hearts Three Nothing)
            (PlayedCard SuitFollowed Hearts Jack Nothing)
            (PlayedCard SuitFollowed Hearts Two Nothing),
          Trick
            (PlayedCard SuitLed Hearts King Nothing)
            (PlayedCard SuitFollowed Hearts Four Nothing)
            (PlayedCard SuitFollowed Hearts Seven Nothing)
            (PlayedCard SuitFollowed Hearts Ace Nothing),
          Trick
            (PlayedCard SuitLed Diamonds Jack Nothing)
            (PlayedCard SuitFollowed Diamonds Ace Nothing)
            (PlayedCard SuitFollowed Diamonds Five Nothing)
            (PlayedCard SuitFollowed Diamonds Three Nothing),
          Trick
            (PlayedCard SuitLed Hearts Ten Nothing)
            (PlayedCard SuitFollowed Hearts Six Nothing)
            (PlayedCard SuitFollowed Hearts Eight Nothing)
            (PlayedCard Discard Clubs Seven (Just $ Note 1 ""))
        ]
  MP.parse playParser "" i1 `shouldParse` result1

testParsePlayWithoutAnnotation :: IO ()
testParsePlayWithoutAnnotation = do
  let i1 = "P SK54T:SA87H3:HA245:HKQ8J\n"
      result1 =
        [ Trick
            (PlayedCard SuitLed Spades King Nothing)
            (PlayedCard SuitFollowed Spades Five Nothing)
            (PlayedCard SuitFollowed Spades Four Nothing)
            (PlayedCard SuitFollowed Spades Ten Nothing),
          Trick
            (PlayedCard SuitLed Spades Ace Nothing)
            (PlayedCard SuitFollowed Spades Eight Nothing)
            (PlayedCard SuitFollowed Spades Seven Nothing)
            (PlayedCard Discard Hearts Three Nothing),
          Trick
            (PlayedCard SuitLed Hearts Ace Nothing)
            (PlayedCard SuitFollowed Hearts Two Nothing)
            (PlayedCard SuitFollowed Hearts Four Nothing)
            (PlayedCard SuitFollowed Hearts Five Nothing),
          Trick
            (PlayedCard SuitLed Hearts King Nothing)
            (PlayedCard SuitFollowed Hearts Queen Nothing)
            (PlayedCard SuitFollowed Hearts Eight Nothing)
            (PlayedCard SuitFollowed Hearts Jack Nothing)
        ]
  MP.parse playParser "" i1 `shouldParse` result1

testPartialContractWithTrickGoal :: IO ()
testPartialContractWithTrickGoal = do
  let i1 = "C CM:W\n"
      result1 =
        Contract
          { contractLevel = TrickGoal,
            suit = Trump Clubs,
            scoreModifier = Nothing,
            contractGoal = Just $ MaximumTricks,
            declarer = West,
            onLead = North
          }
  MP.parse contractParser "" i1 `shouldParse` result1

testContractWithOnLeadPlayer :: IO ()
testContractWithOnLeadPlayer = do
  let i1 = "C H6:S:S\n"
      result1 =
        Contract
          { contractLevel = TrickGoal,
            suit = Trump Hearts,
            scoreModifier = Nothing,
            contractGoal = Just $ NumberOfTricks SixTricks,
            declarer = South,
            onLead = South
          }
  MP.parse contractParser "" i1 `shouldParse` result1

testContractWithGoalsParser :: IO ()
testContractWithGoalsParser = do
  let i1 = "C 4SX8:S\n"
      result1 =
        Contract
          { contractLevel = ContractLevel LevelFour,
            suit = Trump Spades,
            scoreModifier = Just Doubled,
            contractGoal = Just $ NumberOfTricks EightTricks,
            declarer = South,
            onLead = West
          }
  MP.parse contractParser "" i1 `shouldParse` result1

testDoubledAndRedbledContracts :: IO ()
testDoubledAndRedbledContracts = do
  let i1 = "C 6NR:E\n"
      result1 =
        Contract
          { contractLevel = ContractLevel LevelSix,
            suit = NoTrump,
            scoreModifier = Just Redoubled,
            contractGoal = Nothing,
            declarer = East,
            onLead = South
          }
      i2 = "C 7CX:N\n"
      result2 =
        Contract
          { contractLevel = ContractLevel LevelSeven,
            suit = Trump Clubs,
            scoreModifier = Just Doubled,
            contractGoal = Nothing,
            declarer = North,
            onLead = East
          }
  MP.parse contractParser "" i1 `shouldParse` result1
  MP.parse contractParser "" i2 `shouldParse` result2

testUndoubleContractAndDeclarerParser :: IO ()
testUndoubleContractAndDeclarerParser = do
  let i1 = "C 5D:N\n"
      result1 = Contract (ContractLevel LevelFive) (Trump Diamonds) Nothing Nothing North East
  MP.parse contractParser "" i1 `shouldParse` result1

testNotesParser :: IO ()
testNotesParser = do
  let i1 = "1 Lost his mind\n2 His partner was fuming at this call\n"
      result1 = Map.fromList [(1, "Lost his mind"), (2, "His partner was fuming at this call")]
  MP.parse parseNotes "" i1 `shouldParse` result1

testAnnotationNotesParser :: IO ()
testAnnotationNotesParser = do
  let i1 = "A EB:3CP3N?P:PX!R^1A\n1 Lost his mind\n"
      result1 =
        Auction
          East
          BothVul
          ( [ Bid (BidCall LevelThree (Trump Clubs)) Nothing,
              Bid Pass Nothing,
              Bid (BidCall LevelThree NoTrump) (Just Poor),
              Bid Pass Nothing,
              Bid Pass Nothing,
              Bid Double (Just Good),
              Bid Redouble (Just $ Note 1 "Lost his mind")
            ]
              ++ allPass
          )
  MP.parse auctionParser "" i1 `shouldParse` result1

testAnnotationParser :: IO ()
testAnnotationParser = do
  let i1 = "A EB:3C*P3N?P:P??X!RA\n"
      bids1 =
        [ Bid (BidCall LevelThree (Trump Clubs)) (Just Conventional),
          Bid Pass Nothing,
          Bid (BidCall LevelThree NoTrump) (Just Poor),
          Bid Pass Nothing,
          Bid Pass (Just VeryPoor),
          Bid Double (Just Good),
          Bid Redouble Nothing
        ]
          ++ allPass
      result1 =
        Auction
          East
          BothVul
          bids1
  MP.parse auctionParser "" i1 `shouldParse` result1

testYourCallParser :: IO ()
testYourCallParser = do
  let i1 = "A NB:1SXY\n"
      result1 =
        Auction
          North
          BothVul
          [Bid (BidCall LevelOne (Trump Spades)) Nothing, Bid Double Nothing, Bid YourCall Nothing]
  MP.parse auctionParser "" i1 `shouldParse` result1

testPartialAuctionParser :: IO ()
testPartialAuctionParser = do
  let i1 = "A S?:1SP2CP:2D\n"
      result1 =
        Auction
          South
          UnknownVul
          [ Bid (BidCall LevelOne (Trump Spades)) Nothing,
            Bid Pass Nothing,
            Bid (BidCall LevelTwo (Trump Clubs)) Nothing,
            Bid Pass Nothing,
            Bid (BidCall LevelTwo (Trump Diamonds)) Nothing
          ]
  MP.parse auctionParser "" i1 `shouldParse` result1

testAllPassAuctionParser :: IO ()
testAllPassAuctionParser = do
  let i1 = "A SZ:1SP2SP:4SA\n"
      spades = Trump Spades
      bids =
        [ Bid (BidCall LevelOne spades) Nothing,
          Bid Pass Nothing,
          Bid (BidCall LevelTwo spades) Nothing,
          Bid Pass Nothing,
          Bid (BidCall LevelFour spades) Nothing
        ]
          ++ allPass
      result1 = Auction South NoneVul bids
      i2 = "A WE:A\n"
      auction2 = allPass
      result2 = Auction West EastWestVul auction2
  MP.parse auctionParser "" i1 `shouldParse` result1

testParseSameDealDifferentOrientation :: IO ()
testParseSameDealDifferentOrientation = do
  let i1 = "H E;K5.T.KQJT98.KQJT:A876.A2.765.A876;32.KQJ9876543..9:\n"
      i2 = "H S:A876.A2.765.A876;32.KQJ9876543..9:QJT94..A432.5432;\n"
      i3 = "H W;32.KQJ9876543..9:QJT94..A432.5432;K5.T.KQJT98.KQJT:\n"
      i4 = "H N:QJT94..A432.5432;K5.T.KQJT98.KQJT:A876.A2.765.A876;\n"
      westHand1 = makeHand (defaultHandProps {visibility = Hidden}) "32" "KQJ9876543" "" "9"
      northHand1 = makeHand defaultHandProps "QJT94" "" "A432" "5432"
      eastHand1 = makeHand (defaultHandProps {visibility = Hidden}) "K5" "T" "KQJT98" "KQJT"
      southHand1 = makeHand defaultHandProps "A876" "A2" "765" "A876"
      result1 = Map.fromList [(North, northHand1), (East, eastHand1), (South, southHand1), (West, westHand1)]
  MP.parse handsParser "" i1 `shouldParse` result1
  MP.parse handsParser "" i2 `shouldParse` result1
  MP.parse handsParser "" i3 `shouldParse` result1
  MP.parse handsParser "" i4 `shouldParse` result1

testParseHiddenHands :: IO ()
testParseHiddenHands = do
  let i1 = "H W:A875.632.76.8643;Q632.Q87.J842.Q2:KJT4.KJT4.AQ3.AJ;\n"
      i2 = "H W:A8765.QT.K9.AT87;J42.AJ7632.J.632;QT3.85.Q86.KQJ54;\n"

      westHand1 = makeHand defaultHandProps "A875" "632" "76" "8643"
      northHand1 = makeHand (defaultHandProps {visibility = Hidden}) "Q632" "Q87" "J842" "Q2"
      eastHand1 = makeHand defaultHandProps "KJT4" "KJT4" "AQ3" "AJ"
      southHand1 = makeHand (defaultHandProps {visibility = Hidden}) "9" "A95" "KT95" "KT975"
      result1 = Map.fromList [(North, northHand1), (East, eastHand1), (South, southHand1), (West, westHand1)]

      westHand2 = makeHand defaultHandProps "A8765" "QT" "K9" "AT87"
      northHand2 = makeHand (defaultHandProps {visibility = Hidden}) "J42" "AJ7632" "J" "632"
      eastHand2 = makeHand (defaultHandProps {visibility = Hidden}) "QT3" "85" "Q86" "KQJ54"
      southHand2 = makeHand (defaultHandProps {visibility = Hidden}) "K9" "K95" "AT75432" "9"
      result2 =
        Map.fromList [(North, northHand2), (East, eastHand2), (South, southHand2), (West, westHand2)]
  MP.parse handsParser "" i1 `shouldParse` result1

testSingleSuitPartialDeals :: IO ()
testSingleSuitPartialDeals = do
  let i1 = "H W:43:KJ2:Q976:AT85\n"
      westHand1 = makeHand defaultHandProps "43" "" "" ""
      northHand1 = makeHand defaultHandProps "KJ2" "" "" ""
      eastHand1 = makeHand defaultHandProps "Q976" "" "" ""
      southHand1 = makeHand defaultHandProps "AT85" "" "" ""
      result1 = Map.fromList [(North, northHand1), (East, eastHand1), (South, southHand1), (West, westHand1)]
  MP.parse handsParser "" i1 `shouldParse` result1

testPartialDeals :: IO ()
testPartialDeals = do
  let i1 = "H W:K9.K9.9:3.A3.Q.4:Q82..J.A:A7.7.K8\n"
      westHand1 = makeHand defaultHandProps "K9" "K9" "9" ""
      northHand1 = makeHand defaultHandProps "3" "A3" "Q" "4"
      eastHand1 = makeHand defaultHandProps "Q82" "" "J" "A"
      southHand1 = makeHand defaultHandProps "A7" "7" "K8" ""
      result1 = Map.fromList [(North, northHand1), (East, eastHand1), (South, southHand1), (West, westHand1)]
  MP.parse handsParser "" i1 `shouldParse` result1

testSingleHandDeals :: IO ()
testSingleHandDeals = do
  let i1 = "H E:T4.8642.AKT8.K65\n"
      northHand1 = emptyHand
      eastHand1 = makeHand defaultHandProps "T4" "8642" "AKT8" "K65"
      southHand1 = emptyHand
      westHand1 = emptyHand
      result1 = Map.fromList [(North, northHand1), (East, eastHand1), (South, southHand1), (West, westHand1)]
  MP.parse handsParser "" i1 `shouldParse` result1

testParsePairHands :: IO ()
testParsePairHands = do
  let i1 = "H N:AKQ72..AKQ72.753::.AKQ72.753.AKQ72\n"
      i2 = "H W:AKQ72..AKQ72.753::.AKQ72.753.AKQ72\n"
      northHand1 = makeHand defaultHandProps "AKQ72" "" "AKQ72" "753"
      eastHand1 = emptyHand
      southHand1 = makeHand defaultHandProps "" "AKQ72" "753" "AKQ72"
      westHand1 = emptyHand
      westHand2 = makeHand defaultHandProps "AKQ72" "" "AKQ72" "753"
      southHand2 = emptyHand
      eastHand2 = makeHand defaultHandProps "" "AKQ72" "753" "AKQ72"
      northHand2 = emptyHand
      result1 =
        Map.fromList [(North, northHand1), (East, eastHand1), (South, southHand1), (West, westHand1)]
      result2 =
        Map.fromList [(North, northHand2), (East, eastHand2), (South, southHand2), (West, westHand2)]
  MP.parse handsParser "" i1 `shouldParse` result1
  MP.parse handsParser "" i2 `shouldParse` result2

testHandsWithVoids :: IO ()
testHandsWithVoids = do
  let i1 = "H N:AKQJT98765432...:.AKQJT98765432..:..AKQJT98765432.:\n"
      northHand1 = makeHand defaultHandProps "AKQJT98765432" "" "" ""
      eastHand1 = makeHand defaultHandProps "" "AKQJT98765432" "" ""
      southHand1 = makeHand defaultHandProps "" "" "AKQJT98765432" ""
      westHand1 = makeHand defaultHandProps "" "" "" "AKQJT98765432"
      result1 =
        Map.fromList [(North, northHand1), (East, eastHand1), (South, southHand1), (West, westHand1)]
  MP.parse handsParser "" i1 `shouldParse` result1

testHandsParser :: IO ()
testHandsParser = do
  let i1 = "H W:873.A6.KT864.KQ8:96.T54.97.AJ9643:T542.K93.AQ53.52:\n"
      i2 = "H S:9.AK6.AKT982.K87:K7654.J73.Q65.T6:QT2.T94.J4.AQ953:\n"
      westHand1 =
        makeHand defaultHandProps "873" "A6" "KT864" "KQ8"
      northHand1 =
        makeHand defaultHandProps "96" "T54" "97" "AJ9643"
      eastHand1 =
        makeHand defaultHandProps "T542" "K93" "AQ53" "52"
      southHand1 =
        makeHand defaultHandProps "AKQJ" "QJ872" "J2" "T7"
      southHand2 =
        makeHand defaultHandProps "9" "AK6" "AKT982" "K87"
      westHand2 =
        makeHand defaultHandProps "K7654" "J73" "Q65" "T6"
      northHand2 =
        makeHand defaultHandProps "QT2" "T94" "J4" "AQ953"
      eastHand2 =
        makeHand defaultHandProps "AJ83" "Q852" "73" "J42"
      result1 =
        Map.fromList
          [ (North, northHand1),
            (East, eastHand1),
            (South, southHand1),
            (West, westHand1)
          ]
      result2 =
        Map.fromList
          [ (North, northHand2),
            (East, eastHand2),
            (South, southHand2),
            (West, westHand2)
          ]
  MP.parse handsParser "" i1 `shouldParse` result1
  MP.parse handsParser "" i2 `shouldParse` result2

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