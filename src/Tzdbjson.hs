{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}

module Tzdbjson
       ( Parser
       , pRule
       ) where

import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, pack)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Tzdbjson.Types


type Parser = Parsec Void Text

pToYear :: Int -> Parser (Maybe Int)
pToYear fromY = try (Just <$> L.decimal) <|> (string "only" *> pure (Just fromY)) <|> (string "max" *> pure Nothing)

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

pTime :: Parser Int
pTime = do
  h <- L.decimal
  _ <- char ':'
  m <- L.decimal
  -- TODO: add an optional seconds part
  let s = 0
  return $ (h * 60 + m) * 60 + s

pWeekDay :: Parser Int
pWeekDay = choice [ 1 <$ string "Mon"
                  , 2 <$ string "Tue"
                  , 3 <$ string "Wed"
                  , 4 <$ string "Thu"
                  , 5 <$ string "Fri"
                  , 6 <$ string "Sat"
                  , 7 <$ string "Sun"
                  ]

pOperator :: Parser Operator
pOperator = choice [ First <$ string "first"
                   , Last <$ string "last"
                   , Gte <$ string ">="
                   , Lte <$ string "<="
                   ]


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


pAt :: Parser At
pAt = do
  time <- pTime
  suffix <- fromMaybe 'w' <$> optional letterChar
  return At{..}

pSave :: Parser (Maybe Int)
pSave = try (Just <$> pTime) <|> (char '0' *> pure Nothing)

pRule :: Parser Rule
pRule = do
  _ <- string "Rule"
  name <- space1 *> (pack <$> some alphaNumChar)
  fromYear <- space1 *> L.decimal
  toYear <- space1 *> pToYear fromYear
  _ <- space1 *> char '-'
  month <- space1 *> pMonth
  day <- space1 *> pDay
  at <- space1 *> pAt
  save <- space1 *> pSave
  letter <- space1 *> (try letterChar <|> char '-')
  _ <- space *> optional eol
  return Rule{..}
