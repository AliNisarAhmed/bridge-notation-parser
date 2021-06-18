module RBN where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Void
import RIO
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text


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
--   pure c