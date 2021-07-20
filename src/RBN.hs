module RBN where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time
import Data.Void
import Domain.Types
import RIO
import qualified RIO.Map as Map
import RIO.Partial (read, succ, toEnum)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Text.Megaparsec.Debug (dbg)
import Text.Read (Read (readsPrec))

type Parser = Parsec Void Text

---- Result and Score Parser

-- data DealResult = DealResult
--   { numberOfTricks :: Maybe NumberOfTricks,
--     bridgeScore :: Maybe PairPartscore,
--     matchScore :: Maybe Double
--   }
--   deriving (Eq, Show)

-- data PairPartscore
--   = NorthSouthPartscore Int
--   | EastWestParscore Int
--   deriving (Eq, Show)

-- resultParser :: Parser DealResult
-- resultParser = do
--   MPC.char 'R'
--   MPC.space1
--   numTricks <- MP.try (ZeroTricks <$ MPC.c)

---- Play of the hand Parser

playParser :: Parser Play
playParser = do
  MPC.char 'P'
  MPC.space1
  tricks <- MP.many parsePlayRound
  void $ MPC.newline
  notes <- MP.optional parseNotes
  let tricksWithNotes = mergeTricksWithNotes tricks notes
  pure tricksWithNotes

mergeTricksWithNotes :: [Trick] -> Maybe NoteCollection -> [Trick]
mergeTricksWithNotes tricks Nothing = tricks
mergeTricksWithNotes tricks (Just notes) = map merge tricks
  where
    merge :: Trick -> Trick
    merge trick@(Trick p1 p2 p3 p4) =
      let ns = getNotesFromTrick trick
          [n1, n2, n3, n4] = map (updateNote notes) ns
       in Trick (p1{annotation = n1}) (p2{annotation = n2}) (p3{annotation = n3}) (p4{annotation = n4})

updateNote :: NoteCollection -> Maybe Annotation -> Maybe Annotation
updateNote collection (Just (Note n c)) = do
  c <- Map.lookup n collection
  pure $ Note n c
updateNote _ n = n

getNotesFromTrick :: Trick -> [Maybe Annotation]
getNotesFromTrick (Trick c1 c2 c3 c4) =
  map (annotation :: PlayedCard -> Maybe Annotation) [c1, c2, c3, c4]

parsePlayRound :: Parser Trick
parsePlayRound = do
  void $ MP.optional $ MPC.char ':'
  f <- MP.try parseFirstCard <|> parsePseudoPlays
  s <- MP.try $ parseSubsequentCard f.suit <|> parsePseudoPlays
  t <- MP.try $ parseSubsequentCard f.suit <|> parsePseudoPlays
  fourth <- MP.try $ parseSubsequentCard f.suit <|> parsePseudoPlays
  pure $ Trick f s t fourth

parseSubsequentCard :: Suit -> Parser PlayedCard
parseSubsequentCard suit = MP.try parseDiscard <|> parseSuitFollowed suit

parseDiscard :: Parser PlayedCard
parseDiscard = do
  s <- parseSuit
  r <- parseRank
  anno <- MP.optional parseAnnotation
  pure $ PlayedCard Discard s r anno

parseSuitFollowed :: Suit -> Parser PlayedCard
parseSuitFollowed s = do
  r <- parseRank
  anno <- MP.optional parseAnnotation
  pure $ PlayedCard SuitFollowed s r anno

parseFirstCard :: Parser PlayedCard
parseFirstCard = do
  s <- parseSuit
  r <- parseRank
  anno <- MP.optional parseAnnotation
  pure $ PlayedCard SuitLed s r anno

parsePseudoPlays :: Parser PlayedCard
parsePseudoPlays =
  MP.choice
    [ LowestUnplayed <$ MPC.char '-',
      LowestUnplayed <$ MPC.char '~',
      HighestUnplayed <$ MPC.char '+',
      ImmaterialDiscard <$ MPC.char '.'
    ]

parseSuit :: Parser Suit
parseSuit =
  MP.choice
    [ Spades <$ MPC.char 'S',
      Hearts <$ MPC.char 'H',
      Diamonds <$ MPC.char 'D',
      Clubs <$ MPC.char 'C'
    ]

parseRank :: Parser Rank
parseRank =
  MP.choice
    [ Two <$ MPC.char '2',
      Three <$ MPC.char '3',
      Four <$ MPC.char '4',
      Five <$ MPC.char '5',
      Six <$ MPC.char '6',
      Seven <$ MPC.char '7',
      Eight <$ MPC.char '8',
      Nine <$ MPC.char '9',
      Ten <$ MPC.char 'T',
      Jack <$ MPC.char 'J',
      Queen <$ MPC.char 'Q',
      King <$ MPC.char 'K',
      Ace <$ MPC.char 'A'
    ]

---- Contract, Dealer and Opening Leads Parser

contractParser :: Parser Contract
contractParser = do
  MPC.char 'C'
  MPC.space1
  level <- MP.try (ContractLevel <$> parseBidLevel) <|> pure TrickGoal
  suit <- parseTrumpSuit
  jeopardy <- MP.optional parseJeopardy
  goal <- MP.optional parseGoal
  MPC.char ':'
  declarer <- parseDirection
  leader <- MP.try parseOpeningLeader <|> pure (next declarer)
  void MPC.newline
  pure $ Contract level suit jeopardy goal declarer leader
  where
    parseJeopardy :: Parser Jeopardy
    parseJeopardy = MP.choice [Doubled <$ MPC.char 'X', Redoubled <$ MPC.char 'R']
    parseGoal :: Parser ContractGoal
    parseGoal = MP.choice [MaximumTricks <$ MPC.char 'M', NumberOfTricks <$> MPCL.decimal]
    parseOpeningLeader :: Parser Direction
    parseOpeningLeader = do
      MPC.char ':'
      parseDirection

---- Auction Parser

type NoteItem = (Int, Text)

type NoteCollection = Map Int Text

auctionParser :: Parser Auction
auctionParser = do
  MPC.char 'A'
  MPC.space1
  dealer <- parseDirection
  vul <- parseVul
  bids <- parseBids
  notes <- MP.optional parseNotes
  let bidsWithNotes = mergeBidsAndNotes bids notes
  pure $ Auction dealer vul bidsWithNotes
  where
    parseVul :: Parser Vul
    parseVul =
      MP.choice
        [ NoneVul <$ MPC.char 'Z',
          EastWestVul <$ MPC.char 'E',
          NorthSouthVul <$ MPC.char 'N',
          BothVul <$ MPC.char 'B',
          UnknownVul <$ MPC.char '?'
        ]
    parseBids :: Parser [Bid]
    parseBids = do
      bids <- concat <$> MP.some parseBidRound
      passes <- MP.try parseAllPass <|> parseEndOfBids
      pure $ bids ++ passes
    parseEndOfBids = do
      MPC.newline
      pure []
    parseAllPass :: Parser [Bid]
    parseAllPass = do
      MPC.char 'A'
      void MPC.newline
      pure allPass
    parseBidRound :: Parser [Bid]
    parseBidRound = do
      MPC.char ':'
      MP.some parseBid
    parseBid :: Parser Bid
    parseBid = do
      bid <-
        MP.choice
          [ Pass <$ MPC.char 'P',
            Double <$ MPC.char 'X',
            Redouble <$ MPC.char 'R',
            YourCall <$ MPC.char 'Y',
            parseCall
          ]
      anno <- MP.optional parseAnnotation
      pure $ Bid bid anno
    parseCall = do
      level <- parseBidLevel
      trump <- parseTrumpSuit
      pure $ BidCall level trump

parseTrumpSuit :: Parser BidSuit
parseTrumpSuit =
  MP.choice
    [ Trump Spades <$ MPC.char 'S',
      Trump Hearts <$ MPC.char 'H',
      Trump Diamonds <$ MPC.char 'D',
      Trump Clubs <$ MPC.char 'C',
      NoTrump <$ MPC.char 'N'
    ]

parseBidLevel :: Parser BidLevel
parseBidLevel =
  MP.choice
    [ LevelOne <$ MPC.char '1',
      LevelTwo <$ MPC.char '2',
      LevelThree <$ MPC.char '3',
      LevelFour <$ MPC.char '4',
      LevelFive <$ MPC.char '5',
      LevelSix <$ MPC.char '6',
      LevelSeven <$ MPC.char '7'
    ]

allPass :: [Bid]
allPass = [Bid Pass Nothing, Bid Pass Nothing, Bid Pass Nothing]

parseAnnotation :: Parser Annotation
parseAnnotation =
  MP.choice
    [ VeryGood <$ MPC.string "!!",
      VeryPoor <$ MPC.string "??",
      Speculative <$ MPC.string "!?",
      Questionable <$ MPC.string "?!",
      Good <$ MPC.char '!',
      Poor <$ MPC.char '?',
      Conventional <$ MPC.char '*',
      annotationNoteParser
    ]

-- TODO: Refactor
mergeBidsAndNotes :: [Bid] -> Maybe NoteCollection -> [Bid]
mergeBidsAndNotes b Nothing = b
mergeBidsAndNotes bids (Just notes) =
  map updateNote bids
  where
    updateNote :: Bid -> Bid
    updateNote bid =
      let noteItem = getNote bid.annotation
       in case noteItem of
            Nothing -> bid
            Just (n, c) ->
              case Map.lookup n notes of
                Nothing -> bid
                Just noteContent -> bid{annotation = Just $ Note n noteContent}

getNote :: Maybe Annotation -> Maybe NoteItem
getNote (Just (Note i c)) = Just (i, c)
getNote _ = Nothing

annotationNoteParser :: Parser Annotation
annotationNoteParser = do
  MPC.char '^'
  n <- MPCL.decimal
  pure $ Note n ""

parseOneNote :: Parser NoteItem
parseOneNote = do
  n <- MPCL.decimal
  MPC.space1
  c <- MP.takeWhile1P Nothing (/= '\n')
  void MPC.newline
  pure (n, c)

parseNotes :: Parser NoteCollection
parseNotes = do
  notes <- MP.some parseOneNote
  pure $ Map.fromList notes

---- Hands Parser

handsParser :: Parser Deck
handsParser = do
  MPC.char 'H'
  MPC.space1
  startPlayer <- parseDirection
  let player2 = next startPlayer
      player3 = next player2
      player4 = next player3
  h1 <- parseHand
  h2 <- parseHand
  h3 <- parseHand
  temp <- parseHand
  MPC.newline
  let h4 =
        if isPartialDeal h1 h2 h3
          then temp
          else generateFourthHand temp.handProps h1 h2 h3
  pure $ Map.fromList [(startPlayer, h1), (player2, h2), (player3, h3), (player4, h4)]
  where
    parseHand :: Parser Hand
    parseHand = do
      handVisib <- parseHandVisib
      spades <- parseSuit Spades
      hearts <- parseSuit Hearts
      diamonds <- parseSuit Diamonds
      clubs <- parseSuit Clubs
      pure $ Hand (HandProps handVisib) spades hearts diamonds clubs
    parseSuit :: Suit -> Parser (Set Card)
    parseSuit suit = do
      s <- MP.takeWhileP Nothing isCard
      MP.optional $ MPC.char '.'
      pure $ makeHandSuit suit (T.unpack s)
    isCard = (`elem` ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'])

parseDirection :: Parser Direction
parseDirection =
  MP.choice
    [ North <$ MPC.char 'N',
      South <$ MPC.char 'S',
      East <$ MPC.char 'E',
      West <$ MPC.char 'W'
    ]

parseHandVisib :: Parser HandVisibility
parseHandVisib =
  MP.try
    ( do
        MPC.char ';'
        pure Hidden
    )
    <|> ( do
            MP.optional $ MPC.char ':'
            pure Shown
        )

generateFullSuit :: Suit -> Set Card
generateFullSuit s = Set.map (Card s) $ Set.fromList ([minBound .. maxBound] :: [Rank])

generateFourthHand :: HandProps -> Hand -> Hand -> Hand -> Hand
generateFourthHand props (Hand _ s1 h1 d1 c1) (Hand _ s2 h2 d2 c2) (Hand _ s3 h3 d3 c3) =
  let s4 = Set.difference (generateFullSuit Spades) (s1 `Set.union` s2 `Set.union` s3)
      h4 = Set.difference (generateFullSuit Hearts) (h1 `Set.union` h2 `Set.union` h3)
      d4 = Set.difference (generateFullSuit Diamonds) (d1 `Set.union` d2 `Set.union` d3)
      c4 = Set.difference (generateFullSuit Clubs) (c1 `Set.union` c2 `Set.union` c3)
   in Hand props s4 h4 d4 c4

isPartialDeal :: Hand -> Hand -> Hand -> Bool
isPartialDeal h1 h2 h3 =
  (isEmptyHand h1 || isEmptyHand h2 || isEmptyHand h3)
    || (numCardsInHand h1 < 13)

numCardsInHand :: Hand -> Int
numCardsInHand (Hand _ s h d c) = Set.size s + Set.size h + Set.size d + Set.size c

isEmptyHand :: Hand -> Bool
isEmptyHand (Hand _ s h d c) = Set.null s && Set.null h && Set.null d && Set.null c

makeHandSuit :: Suit -> [Char] -> Set Card -- TODO: rename
makeHandSuit s = Set.fromList . map (Card s . charToRank)

makeHand :: HandProps -> [Char] -> [Char] -> [Char] -> [Char] -> Hand
makeHand handProps s h d c =
  Hand handProps (makeHandSuit Spades s) (makeHandSuit Hearts h) (makeHandSuit Diamonds d) (makeHandSuit Clubs c)

emptyHand :: Hand
emptyHand = makeHand defaultHandProps [] [] [] []

charToRank :: Char -> Rank
charToRank c =
  case c of
    'A' -> Ace
    'K' -> King
    'Q' -> Queen
    'J' -> Jack
    'T' -> Ten
    '9' -> Nine
    '8' -> Eight
    '7' -> Seven
    '6' -> Six
    '5' -> Five
    '4' -> Four
    '3' -> Three
    '2' -> Two

---- Board Number Parser

data BoardNumber = BoardNumber {boardNumber :: Text, session :: Maybe Text}
  deriving (Eq, Show)

boardNumberParser :: Parser BoardNumber
boardNumberParser = do
  MPC.char 'B'
  MPC.space1
  bn <- parseBoardNumber
  sn <- parseSession
  pure $ BoardNumber bn sn
  where
    parseBoardNumber :: Parser Text
    parseBoardNumber = do
      n <- MP.takeWhile1P Nothing (not . isSeparator)
      MP.optional $ MPC.char ':'
      pure n
    isSeparator = (`elem` [':', '\n'])
    parseSession :: Parser (Maybe Text)
    parseSession = do
      s <- MP.optional $ MP.takeWhile1P Nothing (/= '\n')
      MPC.newline
      pure s

---- Player Names Parser

data Players = Players
  { north :: Maybe Player,
    south :: Maybe Player,
    east :: Maybe Player,
    west :: Maybe Player,
    tableNumber :: Maybe Int,
    room :: Maybe MatchRoom,
    additionalInfo :: Maybe Text
  }
  deriving (Eq, Show)

playersParser :: Parser Players
playersParser = do
  MPC.char 'N'
  MPC.space1
  (n, s, w, e) <- parsePlayers
  t <- parseTable
  r <- parseRoom
  additional <- parseAdditional
  pure $ Players n s w e t r additional
  where
    parsePlayers :: Parser (Maybe Player, Maybe Player, Maybe Player, Maybe Player)
    parsePlayers = do
      (n, s) <- parsePair
      (w, e) <- parsePair
      let north = Player North <$> n
          south = Player South <$> s
          east = Player East <$> e
          west = Player West <$> w
      pure (north, south, west, east)
      where
        parsePair = do
          n <- MP.optional $ MP.takeWhile1P Nothing (not . isPlayerSeparator)
          MP.optional $ MPC.char '+'
          s <- MP.optional $ MP.takeWhile1P Nothing (not . isPairSeparator)
          MP.optional $ MPC.char ':'
          pure (n, s)
        playerSeparator :: [Char]
        playerSeparator = ['+', '\n', ':']
        isPlayerSeparator = (`elem` playerSeparator)
        pairSeparator :: [Char]
        pairSeparator = [':', '\n']
        isPairSeparator = (`elem` pairSeparator)
    parseTable :: Parser (Maybe Int)
    parseTable = MP.optional MPCL.decimal
    parseRoom :: Parser (Maybe MatchRoom)
    parseRoom =
      MP.optional
        ( MP.try do
            MPC.char 'O'
            pure OpenRoom
            <|> do
              MPC.char 'C'
              pure ClosedRoom
        )
    parseAdditional :: Parser (Maybe Text)
    parseAdditional = MP.optional do
      MPC.char ':'
      s <- MP.takeWhile1P Nothing (/= '\n')
      MPC.newline
      pure s

---- Team Name Parser

data Teams = Teams
  { names :: (Text, Text),
    scores :: Maybe (Double, Double)
  }
  deriving (Eq, Show)

teamsParser :: Parser Teams
teamsParser = do
  MPC.char 'K'
  MPC.space1
  names <- teamNamesParser
  scores <- teamScoresParser
  pure $ Teams names scores
  where
    teamNamesParser :: Parser (Text, Text)
    teamNamesParser = do
      team1 <- MP.manyTill MPCL.charLiteral (MPC.char ':')
      team2 <- MP.manyTill MPCL.charLiteral (MP.try $ MPC.char ':' <|> MPC.newline)
      pure (T.pack team1, T.pack team2)
    teamScoresParser :: Parser (Maybe (Double, Double))
    teamScoresParser = MP.optional do
      s1 <- MP.try MPCL.float <|> MPCL.decimal
      MPC.char ':'
      s2 <- MP.try MPCL.float <|> MPCL.decimal
      MPC.newline
      pure (s1, s2)

---- Scoring Parser

data Scoring
  = IMPs (Maybe ScoreDetail)
  | BoardAMatch (Maybe ScoreDetail)
  | TotalPoints (Maybe ScoreDetail)
  | IMPPairs (Maybe ScoreDetail)
  | Matchpoints (Maybe ScoreDetail)
  | InstantMatchpoints (Maybe ScoreDetail)
  | RubberBridge (Maybe ScoreDetail)
  | Chicago (Maybe ScoreDetail)
  | Cavendish (Maybe ScoreDetail)
  | PlusOrFishfood (Maybe ScoreDetail)
  deriving (Eq, Show)

data ScoreDetail
  = YearOfScoring Int
  | NorthSouthPartscore Int
  | EastWestPartscore Int
  | GeneralScoreDetail Text
  deriving (Eq, Show)

scoringParser :: Parser Scoring
scoringParser = do
  MPC.char 'F'
  MPC.space1
  s <- sp
  s <$> scoreDetailParser
  where
    sp :: Parser (Maybe ScoreDetail -> Scoring)
    sp = do
      charToScoring <$> MPCL.charLiteral
    charToScoring :: Char -> (Maybe ScoreDetail -> Scoring)
    charToScoring c =
      case c of
        'I' -> IMPs
        'B' -> BoardAMatch
        'T' -> TotalPoints
        'X' -> IMPPairs
        'M' -> Matchpoints
        'N' -> InstantMatchpoints
        'R' -> RubberBridge
        'C' -> Chicago
        'A' -> Cavendish
        'P' -> PlusOrFishfood

scoreDetailParser :: Parser (Maybe ScoreDetail)
scoreDetailParser =
  MP.optional
    ( do
        MPC.char ':'
        MP.try yearOfScoring <|> MP.try (pairPartscore "NS") <|> MP.try (pairPartscore "EW") <|> MP.try generalDetail
    )
  where
    yearOfScoring :: Parser ScoreDetail
    yearOfScoring = do
      n <- MPCL.decimal
      void MPC.newline
      pure $ YearOfScoring n
    pairPartscore :: Text -> Parser ScoreDetail
    pairPartscore pair = do
      void $ MPC.string pair
      void MPC.space1
      n <- MPCL.decimal
      void MPC.newline
      pure $ NorthSouthPartscore n
    generalDetail :: Parser ScoreDetail
    generalDetail = do
      s <- MP.manyTill MPCL.charLiteral MPC.newline
      pure $ GeneralScoreDetail (T.pack s)

---- Session Parser

data Session
  = Segment Int
  | Quarterfinal Int
  | Semifinal Int
  | Final Int
  | Playoff Int
  | RoundOfNumber Int (Maybe Int) -- R32:3 == RoundOfNumber 32 (Just 3)
  | Qualifying Int
  | GeneralSession Text Text
  deriving (Eq, Show)

sessionParser :: Parser Session
sessionParser = do
  MPC.char 'S'
  MPC.space1
  MP.try sessionParser' <|> generalSessionParser
  where
    sessionParser' :: Parser Session
    sessionParser' =
      MP.choice
        [ segmentParser,
          roundOfParser,
          knockoutRoundParser
        ]

    generalSessionParser :: Parser Session
    generalSessionParser = do
      s1 <- MP.manyTill MPCL.charLiteral (MPC.char ':')
      s2 <- MP.manyTill MPCL.charLiteral MPC.newline
      pure $ GeneralSession (T.pack s1) (T.pack s2)

    segmentParser :: Parser Session
    segmentParser = do
      n <- MPCL.decimal
      void MPC.newline
      pure $ Segment n

    knockoutRoundParser :: Parser Session
    knockoutRoundParser = do
      c <-
        MP.choice
          [ Final <$ MPC.char 'F',
            Semifinal <$ MPC.char 'S',
            Quarterfinal <$ MPC.char 'Q',
            Playoff <$ MPC.char 'P',
            Qualifying <$ MPC.char 'I'
          ]
      round <- roundParser
      pure $ c round

    roundParser :: Parser Int
    roundParser = do
      MPC.char ':'
      n <- MPCL.decimal
      void MPC.newline
      pure n

    roundOfParser :: Parser Session
    roundOfParser =
      MP.try
        ( do
            n <- rp
            void MPC.newline
            pure $ RoundOfNumber n Nothing
        )
        <|> do
          n1 <- rp
          n2 <- roundParser
          pure $ RoundOfNumber n1 (Just n2)
      where
        rp :: Parser Int
        rp = do
          void $ MPC.char 'R'
          MPCL.decimal

---- Event Parser ----

data Event = Event
  { generic :: Text,
    specific :: Maybe Text
  }
  deriving (Eq, Show)

eventParser :: Parser Event
eventParser = do
  MPC.char 'E'
  MPC.space1
  generic <- genericParser
  specific <- specificParser
  pure $ Event (T.pack generic) (T.pack <$> specific)

----- Location Parser ------

data Location = Location
  { generic :: Text,
    specific :: Maybe Text
  }
  deriving (Eq, Show)

locationParser :: Parser Location
locationParser = do
  MPC.char 'L'
  MPC.space1
  generic <- genericParser
  specific <- specificParser
  pure $ Location (T.pack generic) (T.pack <$> specific)

genericParser :: Parser [Char]
genericParser = MP.manyTill MP.anySingle (MP.choice [MPC.newline, MPC.char ':'])

specificParser :: Parser (Maybe [Char])
specificParser = optional (MP.manyTill MP.anySingle MPC.newline)

---- Article Info Parser -----
data ArticleInfo = ArticleInfo
  { title :: Maybe Text,
    author :: Text
  }
  deriving (Eq, Show)

articleInfoParser :: Parser ArticleInfo
articleInfoParser = do
  MPC.char 'T'
  MPC.space1
  (title, author) <- MP.try parseWithoutTitle <|> parseWithTitle
  pure $ ArticleInfo title author

parseWithoutTitle :: Parser (Maybe Text, Text)
parseWithoutTitle = do
  dbg "parse colon" $ MPC.char ':'
  author <- dbg "many till author" $ MP.manyTill (MP.satisfy (/= '\n')) MPC.newline
  pure (Nothing, T.pack author)

parseWithTitle :: Parser (Maybe Text, Text)
parseWithTitle = do
  title <- MP.manyTill (MP.satisfy (/= ':')) (MPC.char ':')
  author <- MP.manyTill (MP.satisfy (/= '\n')) MPC.newline
  pure (Just $ T.pack title, T.pack author)

---- Date Parser ----

dateParser :: Parser (Maybe Day)
dateParser = do
  MPC.char 'D'
  MPC.space1
  s <- MP.manyTill MPC.digitChar MPC.newline
  pure (parseYYYYMMDD s <|> parseYYYYMM s <|> parseYYYY s)

-- 19980131
parseYYYYMMDD :: String -> Maybe Day
parseYYYYMMDD = parseTimeM True defaultTimeLocale "%Y%m%d"

--- 199807
parseYYYYMM :: String -> Maybe Day
parseYYYYMM = parseTimeM True defaultTimeLocale "%Y%m"

--- 1999

parseYYYY :: String -> Maybe Day
parseYYYY = parseTimeM True defaultTimeLocale "%Y"
