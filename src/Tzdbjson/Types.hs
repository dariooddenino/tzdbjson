{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}

module Tzdbjson.Types
  ( Rule(..)
  , At(..)
  , Day(..)
  , Operator(..)
  ) where

import           Data.Aeson   (ToJSON)
import           Data.Text
import           GHC.Generics

-- | A data type defining a tzdb rule.
-- Values and types are tweaked for simpler usage.
data Rule = Rule { name     :: Text
                 , fromYear :: Int
                 , toYear   :: Maybe Int -- ^ Nothing means there's no end year (yet)
                 , month    :: Int       -- ^ Starting at Jan == 1
                 , day      :: Day
                 , at       :: At
                 , save     :: Maybe Int -- ^ Minutes
                 , letter   :: Char
                 }
  deriving stock (Eq, Show, Generic)
instance ToJSON Rule

data At = At { time   :: Int    -- ^ Minutes after midnight
             , suffix :: Char -- ^ w: wall clock, s: standard (non daylight), g: gmt, u: utc, z: zulu
             }
  deriving stock (Eq, Show, Generic)
instance ToJSON At



data Operator = First | Last | Lte | Gte
  deriving stock (Eq, Show, Generic)
instance ToJSON Operator

data Day = Day { number   :: Maybe Int
               , weekday  :: Maybe Int
               , operator :: Maybe Operator
               }
  deriving stock (Eq, Show, Generic)
instance ToJSON Day
