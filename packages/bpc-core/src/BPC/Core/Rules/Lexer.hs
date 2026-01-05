{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Rules.Lexer - Lexical analysis for Rule DSL
--
-- Handles whitespace, comments, keywords, identifiers, literals, and operators
-- following SSOT 8.1 EBNF grammar.
module BPC.Core.Rules.Lexer
  ( -- * Parser Type
    Parser

    -- * Whitespace & Comments
  , sc
  , lexeme
  , symbol

    -- * Keywords
  , keywords
  , keyword
  , rword

    -- * Identifiers
  , identifier
  , fieldPath

    -- * Literals
  , litBool
  , litInt
  , litString
  , litDec
  , litQty
  , litDate

    -- * Operators
  , operatorTable
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type using Text input and Void for custom errors.
type Parser = Parsec Void Text

-- | Space consumer that handles whitespace and line comments.
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  empty

-- | Wrap a parser to consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a fixed symbol/string.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Set of reserved keywords.
keywords :: Set Text
keywords = Set.fromList
  [ "field", "let", "in", "if", "then", "else"
  , "true", "false", "assert", "example", "property"
  , "forall", "implies", "none", "some"
  ]

-- | Parse a specific keyword.
keyword :: Text -> Parser ()
keyword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | Parse a reserved word (alias for keyword).
rword :: Text -> Parser ()
rword = keyword

-- | Parse an identifier (not a keyword).
identifier :: Parser Text
identifier = lexeme $ try $ do
  name <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  let txt = T.pack name
  if txt `Set.member` keywords
    then fail $ "keyword " ++ show txt ++ " cannot be an identifier"
    else pure txt

-- | Parse a dotted field path like "battery.capacity_kwh".
fieldPath :: Parser Text
fieldPath = lexeme $ do
  parts <- (:) <$> identPart <*> many (char '.' *> identPart)
  pure $ T.intercalate "." parts
  where
    identPart = T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-- | Parse a boolean literal.
litBool :: Parser Bool
litBool = (True <$ keyword "true") <|> (False <$ keyword "false")

-- | Parse an integer literal.
litInt :: Parser Integer
litInt = lexeme L.decimal

-- | Parse a string literal with escape sequences.
litString :: Parser Text
litString = lexeme $ do
  _ <- char '"'
  str <- manyTill L.charLiteral (char '"')
  pure $ T.pack str

-- | Parse a decimal literal like "123.456" returning (scale, scaled_value).
litDec :: Parser (Int, Integer)
litDec = lexeme $ try $ do
  intPart <- some digitChar
  _ <- char '.'
  fracPart <- some digitChar
  let scale = length fracPart
  let value = read (intPart ++ fracPart) :: Integer
  pure (scale, value)

-- | Parse a quantity literal like "100 kg" returning (value, scale, unit).
litQty :: Parser (Integer, Text, Text)
litQty = lexeme $ try $ do
  val <- L.decimal
  _ <- space1
  unit <- identifier
  pure (val, "0", unit)

-- | Parse a date literal like @2024-01-15.
litDate :: Parser Text
litDate = lexeme $ do
  _ <- char '@'
  year <- count 4 digitChar
  _ <- char '-'
  month <- count 2 digitChar
  _ <- char '-'
  day <- count 2 digitChar
  pure $ T.pack $ year ++ "-" ++ month ++ "-" ++ day

-- | Operator precedence table (placeholder for actual implementation).
operatorTable :: [[()]]
operatorTable = []
