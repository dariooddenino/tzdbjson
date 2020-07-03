{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}

module Tzdbjson
       ( someFunc
       , Parser
       , pRule
       ) where

import           Control.Applicative        ((*>))
-- import           Control.Monad
import           Data.Aeson                 (ToJSON)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, pack)
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | A data type defining a tzdb rule.
-- Values and types are tweaked for simpler usage.
data Rule = Rule { name     :: Text
                 , fromYear :: Int
                 , toYear   :: Int
                 , month    :: Int       -- ^ Starting at Jan == 1
                 , day      :: Text
                 , at       :: At
                 , save     :: Maybe Int -- ^ Minutes
                 , letter   :: Char
                 }
  deriving (Eq, Show, Generic, ToJSON)

data At = At { time :: Int    -- ^ Minutes after midnight
             , suffix :: Char -- ^ w: wall clock, s: standard (non daylight), g: gmt, u: utc, z: zulu
             }
  deriving (Eq, Show, Generic, ToJSON)

-- TODO HOW TO ENCODE?
-- | at can have suffix:
-- (w|null) wall clock
-- s standard (non daylight)
-- g gmt
-- u ut/utc
-- z zulu

-- days can either be
-- firstXxx // lastXxx // Xxx>=n // Xxx<=n // n


pToYear :: Int -> Parser Int
pToYear fromY = try L.decimal <|> (string "only" *> pure fromY)

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
  return $ h * 60 + m

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
  day <- space1 *> (pack <$> some alphaNumChar)
  at <- space1 *> pAt
  save <- space1 *> pSave
  letter <- space1 *> (try letterChar <|> char '-')
  _ <- space *> optional eol
  return Rule{..}


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
