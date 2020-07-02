module Main (main) where

import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Tzdbjson

rules = [ "Rule  Chicago 1920 only  -   Jun 13      2:00 1:00 D"
        , "Rule  Chicago 1920 only  -   Jun 13      2:00 1:00 D"
        , "Rule  Chicago 1920 1921  -   Oct lastSun 2:00 0    S"
        , "Rule  Chicago 1921 only  -   Mar lastSun 2:00 1:00 D"
        , "Rule  Chicago 1922 1966  -   Apr lastSun 2:00 1:00 D"
        , "Rule  Chicago 1922 1954  -   Sep lastSun 2:00 0    S"
        , "Rule  Chicago 1955 1966  -   Oct lastSun 2:00 0    S"
        ]

main :: IO ()
main = do
  traverse (parseTest pRule) rules
  pure ()
