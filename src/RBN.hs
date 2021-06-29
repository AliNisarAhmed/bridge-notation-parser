module RBN where

import qualified Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time
import Data.Void
import RIO
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data Player = Player {direction :: Direction, name :: Text}
  deriving (Eq, Show)

data Room
  = Open
  | Closed
  deriving (Eq, Show)

---- Player Names Parser

data Players = Players
  { north :: Maybe Player,
    south :: Maybe Player,
    east :: Maybe Player,
    west :: Maybe Player,
    tableNumber :: Maybe Int,
    room :: Maybe Room,
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
      n <- MP.optional $ MP.someTill MPCL.charLiteral (MP.try colon <|> plusSign <|> MPC.newline)
      s <- MP.optional $ MP.someTill MPCL.charLiteral colon
      w <- MP.optional $ MP.someTill MPCL.charLiteral (MP.try plusSign <|> MPC.newline)
      e <- MP.optional $ MP.someTill MPCL.charLiteral (MP.try MPC.newline <|> colon)
      let north = Player North <$> (T.pack <$> n)
          south = Player South <$> (T.pack <$> s)
          east = Player East <$> (T.pack <$> e)
          west = Player West <$> (T.pack <$> w)
      pure (north, south, west, east)
      where
        plusSign = MPC.char '+'
        colon = MPC.char ':'
    parseTable :: Parser (Maybe Int)
    parseTable = pure Nothing -- TODO
    parseRoom :: Parser (Maybe Room)
    parseRoom =
      MP.optional
        ( MP.try do
            MPC.char 'O'
            pure Open
            <|> do
              MPC.char 'C'
              pure Closed -- TODO
        )
    parseAdditional :: Parser (Maybe Text)
    parseAdditional = pure Nothing -- TODO

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
