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
import           Data.Text                  (Text, pack)
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | A data type defining a tzdb rule.
data Rule = Rule { name     :: Text
                 , fromYear :: Int
                 , toYear   :: Int
                 , month    :: Int
                 , day      :: Text
                 , at       :: Text
                 , save     :: Maybe Text
                 , letter   :: Char
                 }
  deriving (Eq, Show, Generic, ToJSON)

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
                , 11 <$ string "Dec"
                ]

pTime :: Parser Text
pTime = pack <$> do
  h <- some alphaNumChar
  _ <- char ':'
  m <- some alphaNumChar
  return $ h <> ":" <> m

pSave :: Parser (Maybe Text)
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
  at <- space1 *> pTime
  save <- space1 *> pSave
  letter <- space1 *> letterChar
  return Rule{..}


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
