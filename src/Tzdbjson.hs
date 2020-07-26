{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tzdbjson where

import           Control.Monad              (void)
import           Data.Aeson
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text                  (Text, pack)
import           Data.Void
import           Prelude                    hiding (until)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug
import           Tzdbjson.Parser
import           Tzdbjson.Types


{-|
I want to convert this data to JSON : use argonaut
I also want to convert it to PureScript data structures immediatly : use purescript bridge
-}

encodeAllRules_ :: [Rule] -> M.Map Name [Value]
encodeAllRules_ = foldl (\m (n, v) -> M.insertWith (++) n [v] m) M.empty . map (\(n, v) -> (n, toJSON v))

encodeAllZones :: [Zone] -> M.Map Name [Value]
encodeAllZones zs = (map toJSON) <$> M.fromList zs

-- | We flip links so that the target is the key and the source the value.
encodeAllLinks :: [Link] -> M.Map Name Value
encodeAllLinks = M.fromList . map (\(f, t) -> (t, toJSON f))

encodeRegion :: [Rule] -> [Zone] -> [Link] -> Value
encodeRegion rs zs ls =  -- toJSON . encodeAllRules_
  let rs' = toJSON $ encodeAllRules_ rs
      zs' = toJSON $ encodeAllZones zs
      ls' = toJSON $ encodeAllLinks ls
  in object [ "rules" .= rs', "zones" .= zs', "links" .= ls' ]


