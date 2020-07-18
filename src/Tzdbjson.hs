{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Tzdbjson where

import           Control.Monad              (void)
import           Data.Aeson                 (encode, Value, toJSON)
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

encodeRegion :: [Rule] -> Value
encodeRegion = toJSON . encodeAllRules_

-- test zones
-- encode links
-- encode whole file in big single Value
-- start working on the CLI part

-- pAllRules_ :: [Rule] ~> [(Name, Rule_)]
-- pAllZones :: [Zone] ~> [(Name, [Zone_])]
-- pAllLinks :: [Link] ~> [(Name, Name)]

-- create a map for each
-- convert to js

-- create a function that takes an input and runs above

-- createa function that opens a file and passes it to the above

-- create the CLI part
