{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Tzdbjson
       ( Parser
       , pRule
       , pZoneName
       , pZone
       , pUntil
       ) where

import           Control.Monad              (void)
import           Data.Map.Strict            hiding (empty)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text                  (Text, pack)
import           Data.Void
import           Prelude                    hiding (until)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Tzdbjson.Types


-- TODO rewrite using lexeme?
-- https://markkarpov.com/tutorial/megaparsec.htm

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parses the ending year taking a default value.
pToYear :: Int -> Parser (Maybe Int)
pToYear fromY = try (Just <$> L.decimal) <|> (string "only" *> pure (Just fromY)) <|> (string "max" *> pure Nothing)

-- | Parses the month, returning an Int between 1 and 12
pMonth :: Parser Int
pMonth = choice [ 1 <$ string "Jan"
                , 2 <$ string "Feb"
                , 3 <$ string "Mar"
                , 4 <$ string "Apr"
                , 5 <$ string "May"
                , 6 <$ string "Jun"
                , 7 <$ string "Jul"
                , 8 <$ string "Aug"
                , 9 <$ string "Sep"
                , 10 <$ string "Oct"
                , 11 <$ string "Nov"
                , 12 <$ string "Dec"
                ]

-- | The time in seconds.
pTime :: Parser Int
pTime = do
  h <- L.decimal
  _ <- char ':'
  m <- L.decimal
  s <- optional (char ':' *> L.decimal)
  return $ (h * 60 + m) * 60 + (fromMaybe 0 s)

-- | Parses the weekday returning an Int between 1 and 7.
pWeekDay :: Parser Int
pWeekDay = choice [ 1 <$ string "Mon"
                  , 2 <$ string "Tue"
                  , 3 <$ string "Wed"
                  , 4 <$ string "Thu"
                  , 5 <$ string "Fri"
                  , 6 <$ string "Sat"
                  , 7 <$ string "Sun"
                  ]

-- | Parses the operator used by the day column.
pOperator :: Parser Operator
pOperator = choice [ First <$ string "first"
                   , Last <$ string "last"
                   , Gte <$ string ">="
                   , Lte <$ string "<="
                   ]

-- | Parses the day.
pDay :: Parser Day
pDay = do
  let pNum = (\n -> Day (Just n) Nothing Nothing) <$> L.decimal
      pWeek = (\n -> Day Nothing (Just n) Nothing) <$> pWeekDay
      flDay = do
        op <- pOperator
        d <- pWeekDay
        return $ Day Nothing (Just d) (Just op)
      compDay = do
        d <- pWeekDay
        op <- pOperator
        n <- L.decimal
        return $ Day (Just n) (Just d) (Just op)

  try compDay <|> try flDay <|> try pWeek <|> pNum

-- | Parses the time at which the daylight switch happens.
pAt :: Parser At
pAt = do
  time <- pTime
  suffix <- fromMaybe 'w' <$> optional letterChar
  return At{..}

-- | Parses how much time is saved.
pSave :: Parser (Maybe Int)
pSave = try (Just <$> pTime) <|> (char '0' *> pure Nothing)

-- | Parses the whole rule.
pRule :: Parser Rule
pRule = do
  _ <- symbol "Rule"
  name <- pack <$> lexeme (some alphaNumChar)
  fromYear <- lexeme L.decimal
  toYear <- lexeme $ pToYear fromYear
  _ <- symbol "-"
  month <- lexeme pMonth
  day <- lexeme pDay
  at <- lexeme pAt
  save <- lexeme pSave
  letter <- lexeme (letterChar <|> char '-')
  _ <- optional eol
  return (name, Rule_{..})

-- Zone parsing code.
-- The last 4 columns are common to all rows, while the first two are present
-- only in the first one.

-- | Parses the UNTIL column for zones
pUntil :: Parser Until
pUntil = do
  year <- lexeme L.decimal
  month <- fromMaybe 1 <$> optional (lexeme pMonth)
  day <- fromMaybe 1 <$> (optional (lexeme L.decimal))
  at <- fromMaybe (At 0'w') <$> (optional (lexeme pAt))
  pure Until{..}

-- | Parses the common section of a rule
pZone_ :: Parser Zone_
pZone_ = do
  m <- optional (char '-')
  stdoff' <- lexeme pTime
  rule <- lexeme $ (const Nothing <$> char '-') <|> (Just . pack <$> some alphaNumChar)
  format <- pack <$> (lexeme $ some (alphaNumChar <|> char '%'))
  until <- Just <$> lexeme pUntil
  -- _ <- optional eol
  let stdoff = if isJust m then (stdoff' * (-1)) else stdoff'
  pure Zone_{..}

pZoneName :: Parser Text
pZoneName = L.nonIndented scn (string "Zone" *> space1 *> (pack <$> some (alphaNumChar <|> char '/' <|> char '_')))

-- | Parses a Zones block
pZone :: Parser Zone
pZone = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
     name <- lexeme pZoneName
     z1 <- pZone_
     return (L.IndentMany Nothing (\zs -> return $ singleton name (z1 : zs)) pZone_)

-- pItemList :: Parser (String, [String])
-- pItemList = L.nonIndented scn (L.indentBlock scn p)
--   where
--     p = do
--       header <- pItem
--       f <- pItem
--       return (L.IndentMany Nothing (\vs -> return (header, f:vs)) pItem)

-- pItem :: Parser String
-- pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

-- parse each rule block as [(Name, Rule_)] ~> maybe better as Map Name [Rule_]
-- parse all zones as Map Name [Zone_]
-- at the end of the file regroup everything in two maps and parse to json

