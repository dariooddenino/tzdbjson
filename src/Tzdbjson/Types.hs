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

{-
{
  "rules": { "ruleName": [`Rule`] },
  "zones": { "zoneName": [`Zone`] }
}
-}


-- | A data type defining a tzdb rule.
-- Values and types are tweaked for simpler usage.
data Rule = Rule { name     :: Text
                 , fromYear :: Int
                 , toYear   :: Maybe Int -- ^ Nothing means there's no end year (yet)
                 , month    :: Int       -- ^ Starting at Jan == 1
                 , day      :: Day
                 , at       :: At
                 , save     :: Maybe Int -- ^ Seconds
                 , letter   :: Char
                 }
  deriving stock (Eq, Show, Generic)
instance ToJSON Rule

-- ^ The time at which the rule starts
data At = At { time   :: Int    -- ^ Seconds after midnight
             , suffix :: Char -- ^ w: wall clock, s: standard (non daylight), g: gmt, u: utc, z: zulu
             }
  deriving stock (Eq, Show, Generic)
instance ToJSON At


-- | An operator used to define the starting day of a rule
data Operator = First | Last | Lte | Gte
  deriving stock (Eq, Show, Generic)
instance ToJSON Operator

-- | The starting day of a rule
data Day = Day { number   :: Maybe Int -- ^ Day number
               , weekday  :: Maybe Int -- ^ Weekday number
               , operator :: Maybe Operator -- ^ The operator
               }
  deriving stock (Eq, Show, Generic)
instance ToJSON Day

-- | A data type to define a zone
data Zone = Zone { name :: Text -- ^ The zone name
                 , stdoff :: Text -- ^ The standard offset in seconds from midnight in seconds
                 , rule:: Text -- ^ The rule name or just an offset
                 , format :: Text -- ^ The zone format
                 , until :: Text -- ^ The date time until this zone is effective
                 }
  deriving stock (Eq, Show, Generic)
instance ToJSON Zone
