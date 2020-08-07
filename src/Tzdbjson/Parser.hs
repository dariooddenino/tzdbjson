{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Tzdbjson.Parser
       ( Parser
       , pRule_
       , pRules_
       , pZoneHead
       , pZone
       , pUntil
       , pAllRules_
       , pAllZones
       , pAllLinks
       ) where

import           Control.Monad              (void, join)
import           Data.Maybe                 (fromMaybe, isJust, isNothing)
import           Data.Text                  (Text, pack)
import           Data.Void
import           Prelude                    hiding (until)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug
import           Tzdbjson.Types

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | A custom implementation which accepts indentation > than the initial value.
indentBlock :: (MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume indentation (white space)
  -> m (L.IndentOpt m a b) -- ^ How to parse “reference” token
  -> m a
indentBlock sc r = do
  sc
  ref <- L.indentLevel
  a   <- r
  case a of
    L.IndentNone x -> x <$ sc
    L.IndentMany indent f p -> do
      mlvl <- (optional . try) (eol *> L.indentGuard sc GT ref)
      done <- isJust <$> optional eof
      case (mlvl, done) of
        (Just lvl, False) ->
          indentedItems ref (fromMaybe lvl indent) sc p >>= f
        _ -> sc *> f []
    L.IndentSome indent f p -> do
      pos <- eol *> L.indentGuard sc GT ref
      let lvl = fromMaybe pos indent
      x <- if | pos <= ref -> L.incorrectIndent GT ref pos
              | otherwise -> p
      xs  <- indentedItems ref lvl sc p
      f (x:xs)
{-# INLINEABLE indentBlock #-}

indentedItems :: MonadParsec e s m
  => Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
  -> m ()              -- ^ How to consume indentation (white space)
  -> m b               -- ^ How to parse indented tokens
  -> m [b]
indentedItems ref lvl sc p = go
  where
    go = do
      sc
      pos  <- L.indentLevel
      done <- isJust <$> optional eof
      if done
        then return []
        else if | pos <= ref -> return []
                | otherwise -> (:) <$> p <*> go


-- | Skips a whole line.
skipLine :: Parser ()
skipLine = () <$ manyTill anySingle newline

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
  m <- optional (char '-')
  t <- try pTime' <|> L.decimal
  return $ if isJust m then ((* (-1)) t) else t
  where
    pTime' = do
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
pRule_ :: Parser (Name, Rule_)
pRule_ = do
  _ <- symbol "Rule"
  name <- pack <$> lexeme (some (alphaNumChar <|> char '-'))
  fromYear <- lexeme L.decimal
  toYear <- lexeme $ pToYear fromYear
  _ <- symbol "-"
  month <- lexeme pMonth
  day <- lexeme pDay
  at <- lexeme pAt
  save <- lexeme pSave
  letter <- pack <$> many (alphaNumChar <|> char '-' <|> char '+')
  _ <- optional eol
  return (name, Rule_{..})

-- | Parses a Rules block until it finds an empty line or a comment line or end of file.
pRules_ :: Parser [Rule]
pRules_ =
  someTill (L.nonIndented scn pRule_) (() <$ eof <|> () <$ newline <|> () <$ lineComment)

-- | Parses all the rules in a file.
pAllRules_ :: Parser [Rule]
pAllRules_ = pInner []
  where
    pInner :: [Rule] -> Parser [Rule]
    pInner acc =
      (try (lookAhead (symbol "Rule")) *> pRules_ >>= \r' -> pInner (acc ++ r'))
      <|> (try eof >> pure acc)
      <|> (skipLine >> pInner acc)

-- Zone parsing code.
-- The last 4 columns are common to all rows, while the first two are present
-- only in the first one.

-- | Parses the UNTIL column for zones
pUntil :: Parser Until
pUntil = do
  year <- lexeme L.decimal
  month <- fromMaybe 1 <$> optional (lexeme pMonth)
  day <- fromMaybe (Day (Just 1) Nothing Nothing) <$> (optional (lexeme pDay))
  -- day <- fromMaybe 1 <$> (optional (lexeme L.decimal))
  at <- fromMaybe (At 0 'w') <$> (optional (lexeme pAt))
  pure Until{..}

-- | Parses a Zone's rulename
pRulename :: Parser (Maybe Text)
pRulename = (const Nothing <$> char '-') <|> (Just . pack <$> some (alphaNumChar <|> char '-'))

-- | Parses a Zone format
pFormat :: Parser Text
pFormat = pack <$> (some (alphaNumChar <|> char '%' <|> char '/' <|> char '-' <|> char '+'))

-- | Parses the common section of a rule
pZone_ :: Parser Zone_
pZone_ = do
  stdoff <- lexeme pTime
  offset <- optional $ try $ lexeme pTime
  rule <- if isNothing offset
    then lexeme pRulename
    else return Nothing
  format <- lexeme pFormat
  until <- (Just <$> lexeme pUntil) <|> (Nothing <$ eol)
  pure Zone_{..}

pZoneName :: Parser Text
pZoneName = pack <$> some (alphaNumChar <|> char '/' <|> char '_' <|> char '-')

pZoneHead :: Parser Text
pZoneHead = L.nonIndented scn (symbol "Zone" *> pZoneName)

-- | Parses a Zones block
pZone :: Parser Zone
pZone = L.nonIndented scn (indentBlock scn p)
  where
    p = do
     name <- lexeme pZoneHead
     z1 <- pZone_
     return (L.IndentMany Nothing (\zs -> return $ (,) name (z1 : zs)) pZone_)

-- | Parses all the zones in a file.
pAllZones :: Parser [Zone]
pAllZones = pInner []
  where
    pInner :: [Zone] -> Parser [Zone]
    pInner acc =
      (try (lookAhead $ symbol "Zone") *> pZone >>= \z' -> pInner (acc ++ [z']))
      <|> (try eof >> pure acc)
      <|> (skipLine >> pInner acc)

-- | Parses a single link
pLink :: Parser Link
pLink = do
  _ <- symbol "Link"
  from <- lexeme pZoneName
  to <- lexeme pZoneName
  return (from, to)

pLinks :: Parser [Link]
pLinks =
  someTill (L.nonIndented sc pLink) (() <$ eof <|> () <$ newline <|> () <$ lineComment)

-- | Parses all the links in a file.
pAllLinks :: Parser [Link]
pAllLinks = pInner []
  where
    pInner :: [Link] -> Parser [Link]
    pInner acc =
          (try pZone >> pInner acc)
      <|> (try (lookAhead $ symbol "Link") *> pLinks >>= \l' -> pInner (acc ++ l'))
      <|> (try eof >> pure acc)
      <|> (skipLine >> pInner acc)
