{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tzdbjson where

import           Control.Monad              (void, forM_)
import           Data.Aeson
import           Data.Bifunctor             (first)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.Either.Validation     as V
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text                  (Text, pack)
import           Data.Void
import           Prelude                    hiding (until)
import           System.IO                  (readFile)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug
import           Tzdbjson.Parser
import           Tzdbjson.Types


{-|
TODO: right now the parser ALWAYS succeed by just skipping malformed values.

What I actually want is for it fail in case it's parsing a "valid" element

I have NO idea of how to do this. Need HALP
-}

encodeAllRules_ :: [Rule] -> M.Map Name [Value]
encodeAllRules_ = foldl (\m (n, v) -> M.insertWith (++) n [v] m) M.empty . map (\(n, v) -> (n, toJSON v))

encodeAllZones :: [Zone] -> M.Map Name [Value]
encodeAllZones zs = (map toJSON) <$> M.fromList zs

-- | We flip links so that the target is the key and the source the value.
encodeAllLinks :: [Link] -> M.Map Name Value
encodeAllLinks = M.fromList . map (\(f, t) -> (t, toJSON f))

encodeRegion :: [Rule] -> [Zone] -> [Link] -> Value
encodeRegion rs zs ls =
  let rs' = toJSON $ encodeAllRules_ rs
      zs' = toJSON $ encodeAllZones zs
      ls' = toJSON $ encodeAllLinks ls
  in object [ "rules" .= rs', "zones" .= zs', "links" .= ls' ]

encodeFile :: String -> IO (V.Validation TzdbParseError ByteString)
encodeFile filePath = do
  fileContent <- readFile filePath
  let parse' p = V.eitherToValidation $ first (\a -> [a]) $ parse p filePath fileContent
  case (,,) <$> parse' pAllRules_ <*> parse' pAllZones <*> parse' pAllLinks of
    V.Failure e            -> return $ V.Failure $ TzdbError e
    V.Success ([], [], []) -> return $ V.Failure $ TzdbEmpty
    V.Success (r', z', l') -> return $ V.Success $ encode $ encodeRegion r' z' l'

-- | Prints all the errors
printErrors :: FilePath -> TzdbParseError -> IO ()
printErrors filePath TzdbEmpty =
  putStrLn $ "WARNING: " <> filePath <> " contains no tzdb data."
printErrors filePath (TzdbError errs) =
  forM_ errs $ \e -> putStrLn $ "ERROR: " <> filePath <> " " <> errorBundlePretty e
