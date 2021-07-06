module RBN where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time
import Data.Void
import RIO
import RIO.Partial (read, succ)
import qualified RIO.Map as Map
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded, CyclicEnum)

data Player = Player {direction :: Direction, name :: Text}
  deriving (Eq, Show)

data MatchRoom
  = OpenRoom
  | ClosedRoom
  deriving (Eq, Show)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Ord, Show, Bounded, Enum)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Eq, Ord, Show)

data Hand = Hand
  { spades :: Set Card,
    hearts :: Set Card,
    diamonds :: Set Card,
    clubs :: Set Card
  }
  deriving (Eq, Ord, Show)

data PlayerHand = PlayerHand Direction Hand
  deriving (Eq, Show)

data Deck = Deck
  { north :: PlayerHand,
    east :: PlayerHand,
    south :: PlayerHand,
    west :: PlayerHand
  }
  deriving (Eq, Show)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  next :: a -> a
  next v
    | v == maxBound = minBound
    | otherwise = succ v

---- Hands Parser

handsParser :: Parser Deck
handsParser = do
  MPC.char 'H'
  MPC.space1
  startPlayer <- parseStartPlayer
  let player2 = next startPlayer
      player3 = next player2
      player4 = next player3
  h1 <- parseHand
  h2 <- parseHand
  h3 <- parseHand
  h4 <- parseHand <|> pure (generateFourthHand h1 h2 h3)
  MPC.newline
  pure $ Deck (PlayerHand startPlayer h1) (PlayerHand player2 h2) (PlayerHand player3 h3) (PlayerHand player4 h4)
  where
    parseStartPlayer :: Parser Direction
    parseStartPlayer = do
      c <- MP.oneOf ['N', 'S', 'E', 'W']
      pure $ charToDirection c
    parseHand :: Parser Hand
    parseHand = do
      MPC.char ':'
      spades <- parseSuit Spades
      hearts <- parseSuit Hearts
      diamonds <- parseSuit Diamonds
      clubs <- parseSuit Clubs
      pure $ Hand spades hearts diamonds clubs
    parseSuit :: Suit -> Parser (Set Card)
    parseSuit suit = do
      s <- MP.takeWhileP Nothing isCard
      MP.optional $ MPC.char '.'
      pure $ makeHandSuit suit (T.unpack s)
    isCard = (`elem` ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'])

generateFullSuit :: Suit -> Set Card
generateFullSuit s = Set.map (Card s) $ Set.fromList ([minBound .. maxBound] :: [Rank])

generateFourthHand :: Hand -> Hand -> Hand -> Hand
generateFourthHand (Hand s1 h1 d1 c1) (Hand s2 h2 d2 c2) (Hand s3 h3 d3 c3) =
  let s4 = Set.difference (generateFullSuit Spades) (s1 `Set.union` s2 `Set.union` s3)
      h4 = Set.difference (generateFullSuit Hearts) (h1 `Set.union` h2 `Set.union` h3)
      d4 = Set.difference (generateFullSuit Diamonds) (d1 `Set.union` d2 `Set.union` d3)
      c4 = Set.difference (generateFullSuit Clubs) (c1 `Set.union` c2 `Set.union` c3)
   in Hand s4 h4 d4 c4

makeHandSuit :: Suit -> [Char] -> Set Card -- TODO: rename
makeHandSuit s = Set.fromList . map (Card s . charToRank)

makeHand :: [Char] -> [Char] -> [Char] -> [Char] -> Hand
makeHand s h d c =
  Hand (makeHandSuit Spades s) (makeHandSuit Hearts h) (makeHandSuit Diamonds d) (makeHandSuit Clubs c)

emptyHand :: Hand
emptyHand = makeHand [] [] [] []

charToDirection :: Char -> Direction
charToDirection c =
  case c of
    'N' -> North
    'S' -> South
    'E' -> East
    'W' -> West

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
      c <- MPCL.charLiteral
      round <- roundParser
      pure $ charToSession c round

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

    charToSession :: Char -> (Int -> Session)
    charToSession 'F' = Final
    charToSession 'S' = Semifinal
    charToSession 'Q' = Quarterfinal
    charToSession 'P' = Playoff
    charToSession 'I' = Qualifying

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

-- parseWithTitle :: Parser (Maybe Text, Text)
-- parseWithTitle = do
--   title <- dbg "manytill title" $ MP.manyTill (MP.try nonColon <|> doubleColon) (MPC.char ':')
--   author <- MP.manyTill (MP.satisfy (/= '\n')) MPC.newline
--   pure (Just $ T.pack title, T.pack author)
--   where
--     nonColon = MP.satisfy (/= ':')
--     doubleColon = do
--       MP.notFollowedBy (MPC.char ':')
--       c1 <- MPC.char ':'
--       c2 <- MPC.char ':'
--       pure c1

-- doubleColonParser :: Parser Char
-- doubleColonParser = do
--   dbg "not followed by" $ MP.notFollowedBy (MPC.char ':')
--   c <- dbg "parsing first colon" $ (MP.satisfy (/= ':'))
