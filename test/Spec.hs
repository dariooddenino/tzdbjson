{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Data.Map.Strict
import           Data.Text             (Text)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.RawString.QQ
import           Tzdbjson
import           Tzdbjson.Types



rules :: Text
rules = [r|Rule  Algeria   1916  only  -   Jun   14  23:00s  1:00  S
Rule	Algeria	1916	1919	-	Oct	Sun	23:00	0	-
Rule	Algeria	1917	only	-	Mar	24	23:00	1:00	S
Rule	Algeria	1918	only	-	Mar	 9	23:00	1:00	S
Rule	Algeria	1919	only	-	Mar	 1	23:00	1:00	S
Rule	Algeria	1920	only	-	Feb	14	23:00	1:00	S
Rule	Algeria	1920	only	-	Oct	23	23:00	0	-
Rule	Algeria	1921	only	-	Mar	14	23:00	1:00	S
Rule	Algeria	1921	only	-	Jun	21	23:00	0	-
Rule	Algeria	1939	only	-	Sep	11	23:00	1:00	S
Rule	Algeria	1939	only	-	Nov	19	 1:00	0	-
Rule	Algeria	1944	1945	-	Apr	Mon	 2:00	1:00	S
Rule	Algeria	1944	only	-	Oct	 8	 2:00	0	-
Rule	Algeria	1945	only	-	Sep	16	 1:00	0	-
Rule	Algeria	1971	only	-	Apr	25	23:00	1:00	S
Rule	Algeria	1971	only	-	Sep	26	23:00	0	-
Rule	Algeria	1977	only	-	May	 6	 0:00	1:00	S
Rule	Algeria	1977	only	-	Oct	21	 0:00	0	-
Rule	Algeria	1978	only	-	Mar	24	 1:00	1:00	S
Rule	Algeria	1978	only	-	Sep	22	 3:00	0	-
Rule	Algeria	1980	only	-	Apr	25	 0:00	1:00	S
Rule	Algeria	1980	only	-	Oct	31	 2:00	0	-
# Shanks & Pottenger give 0:09:20 for Paris Mean Time; go with Howse's
# more precise 0:09:21.
|]


rule1 :: Text
rule1 = "Rule   Algeria   1921  only  -   Mar   14  23:00   1:00  S"
rule2 :: Text
rule2 = "Rule   Algeria   1921  1923  -   Jun   Sun  23:00s   0   -"
rule3 :: Text
rule3 = "Rule   Algeria   1939  max   -   Sep   lastSun  23:00u   1:00  S"
rule4 :: Text
rule4 = "Rule   Algeria   1939  max   -   Sep   Sun>=12  23:00u   1:00  S"
rule5 :: Text
rule5 = "Rule   Algeria   1939  max   -   Sep   Mon<=3  23:00u   1:00  S"

zone1 :: Text
zone1 = [r|Zone Atlantic/Cape_Verde -1:34:04 -	LMT	1912 Jan 01  2:00u # Praia
|]

-- TODO this matches the first rule only :/
-- Copy, and partially apply more elements until it works?
zones :: Text
zones = [r|# more precise 0:09:21.
# Zone  NAME    STDOFF  RULES   FORMAT  [UNTIL]
Zone  Africa/Algiers  0:12:12 -   LMT   1891 Mar 16 # hello
      0:09:21   -   PMT   1911 Mar 11
      0:00  Algeria   WE%sT   1940 Feb 25  2:00
      1:00  Algeria   CE%sT   1946 Oct  7
      0:00  -   WET   1956 Jan 29
      1:00  -   CET   1963 Apr 14
      0:00  Algeria   WE%sT   1977 Oct 21
      1:00  Algeria   CE%sT   1979 Oct 26
      0:00  Algeria   WE%sT   1981 May
      1:00  -   CET

# Angola
|]

szones :: Text
szones = [r|# more precise
Zone Africa/Algiers 0:12:12 - LMT 1891
  0:09:21 - PMT 1911
  0:00 Algeria WE%sT 1940
  -1:00 Algeria CE%sT 1946
  0:00 - WET 1956
|]

mlist :: Text
mlist = [r|
something zero #comment
      one
      two
      three
|]

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' r' = parse r' ""

main :: IO ()
main = hspec $ do
  describe "Rules" $ do
    it "parses a series of rules" $ do
      parse' (many pRule) rules `parseSatisfies` ((== 22) . length)
    it "parses a 'only' end year" $ do
      parse' pRule rule1 `parseSatisfies` (\(_, Rule_{..}) -> toYear == Just 1921)
    it "parses an ending year" $ do
      parse' pRule rule2 `parseSatisfies` (\(_, Rule_{..}) -> toYear == Just 1923)
    it "parses a missing ending year" $ do
      parse' pRule rule3 `parseSatisfies` (\(_, Rule_{..}) -> toYear == Nothing)

    it "parses at without letter" $ do
      parse' pRule rule1 `parseSatisfies` (\(_, Rule_{..}) -> at == At (23 * 60 * 60) 'w')
    it "parses at with a letter" $ do
      parse' pRule rule2 `parseSatisfies` (\(_, Rule_{..}) -> at == At (23 * 60 * 60) 's')

    it "parses the save field" $ do
      parse' pRule rule1 `parseSatisfies` (\(_, Rule_{..}) -> save == Just (1 * 60 * 60))

    it "parses a numeric day" $ do
      parse' pRule rule1 `parseSatisfies` (\(_, Rule_{..}) -> day == Day (Just 14) Nothing Nothing)
    it "parses a week day" $ do
      parse' pRule rule2 `parseSatisfies` (\(_, Rule_{..}) -> day == Day Nothing (Just 7) Nothing)
    it "parses lastSun" $ do
      parse' pRule rule3 `parseSatisfies` (\(_, Rule_{..}) -> day == Day Nothing (Just 7) (Just Last))
    it "parses day after num" $ do
      parse' pRule rule4 `parseSatisfies` (\(_, Rule_{..}) -> day == Day (Just 12) (Just 7) (Just Gte))
    it "parses day before num" $ do
      parse' pRule rule5 `parseSatisfies` (\(_, Rule_{..}) -> day == Day (Just 3) (Just 1) (Just Lte))

  describe "Zones" $ do
    -- it "parses the zone name" $ do
    --   parse' pZoneName "Zone Atlantic/Cape_Verde" `parseSatisfies` ((==) "Atlantic/Cape_Verde")

    -- it "parses an until with only a year" $ do
    --   parse' pUntil "1911" `parseSatisfies` ((==) (Until 1911 1 1 (At 0 'w')))

    -- it "parses an until without time" $ do
    --   parse' pUntil "1922 Mar 10" `parseSatisfies` ((==) (Until 1922 3 10 (At 0 'w')))

    -- it "parses the first zone line" $ do
    --   parse' pZone zone1 `parseSatisfies` ((==) (singleton "Atlantic/Cape_Verde" [Zone_ (-5644) Nothing "LMT" (Just $ Until 1912 (Just 1) (Just 1) (Just $ At 7200 'u'))]))

    it "parses many zones" $ do
      -- parse' pItemList mlist `parseSatisfies` ((==) ("something", ["zero", "one", "two", "three"]))
      -- parse' pZone zones `parseSatisfies` (\m -> (length <$> (m !? "Africa/Algiers")) == Just 10)
      parse' pZone szones `parseSatisfies` (\m -> (length <$> (m !? "Africa/Algiers")) == Just 5)
