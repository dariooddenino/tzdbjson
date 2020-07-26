{- |
Copyright: (c) 2020 Dario Oddenino
SPDX-License-Identifier: MIT
Maintainer: Dario Oddenino <branch13@gmail.com>

See README for more info
-}

module Tzdbjson.Types
  ( Rule_(..)
  , Rule
  , At(..)
  , Day(..)
  , Operator(..)
  , Until(..)
  , Zone_(..)
  , Zone
  , Name
  , Link
  , TzdbParseError(..)
  ) where

import           Data.Aeson   (ToJSON)
import           Data.Text
import           GHC.Generics
import           Text.Megaparsec.Error
import           Data.Void

data TzdbParseError = TzdbEmpty | TzdbError [ParseErrorBundle String Void]

type Name = Text

-- | A data type defining a tzdb rule.
-- Values and types are tweaked for simpler usage.
data Rule_ = Rule_ { fromYear :: Int
                   , toYear   :: Maybe Int -- ^ Nothing means there's no end year (yet)
                   , month    :: Int       -- ^ Starting at Jan == 1
                   , day      :: Day
                   , at       :: At
                   , save     :: Maybe Int -- ^ Seconds
                   , letter   :: Text -- TODO: dash could be Nothing?
                   }
  deriving stock (Eq, Show, Generic)
instance ToJSON Rule_

type Rule = (Name, Rule_)

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

data Until = Until { year :: Int
                   , month :: Int
                   , day :: Int
                   , at :: At
                   }
  deriving stock (Eq, Show, Generic)
instance ToJSON Until

-- | A data type to define a zone
data Zone_ = Zone_  { stdoff :: Int -- ^ The standard offset in seconds from midnight in seconds
                    , rule:: Maybe Text -- ^ The rule name or just an offset
                    , format :: Text -- ^ The zone format
                    , until :: Maybe Until -- ^ The date time until this zone is effective
                    }
  deriving stock (Eq, Show, Generic)
instance ToJSON Zone_

type Zone = (Name, [Zone_])

-- | Links a Zone to another one
type Link = (Name, Name)
