{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.RawString.QQ
import           Tzdbjson
import           Tzdbjson.Parser
import           Tzdbjson.Types



rules :: String
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

rule1 :: String
rule1 = "Rule   Algeria   1921  only  -   Mar   14  23:00   1:00  S"
rule2 :: String
rule2 = "Rule   Algeria   1921  1923  -   Jun   Sun  23:00s   0   -"
rule3 :: String
rule3 = "Rule   Algeria   1939  max   -   Sep   lastSun  23:00u   1:00  S"
rule4 :: String
rule4 = "Rule   Algeria   1939  max   -   Sep   Sun>=12  23:00u   1:00  S"
rule5 :: String
rule5 = "Rule   Algeria   1939  max   -   Sep   Mon<=3  23:00u   1:00  S"

zone1 :: String
zone1 = [r|Zone Atlantic/Cape_Verde -1:34:04 -	LMT	1912 Jan 01  2:00u # Praia
|]

zone2 :: String
zone2 = [r|Zone Atlantic/Cape_Verde -1:34:04 1:00	LMT	1912 Jan 01  2:00u # Praia
|]

-- TODO this matches the first rule only :/
-- Copy, and partially apply more elements until it works?
zones :: String
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

zones2 :: String
zones2 = [r|
# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone Atlantic/Cape_Verde -1:34:04 -	LMT	1912 Jan 01  2:00u # Praia
			-2:00	-	-02	1942 Sep
			-2:00	1:00	-01	1945 Oct 15
			-2:00	-	-02	1975 Nov 25  2:00
			-1:00	-	-01
|]

links :: String
links = [r|
# Côte d'Ivoire / Ivory Coast
# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone	Africa/Abidjan	-0:16:08 -	LMT	1912
			 0:00	-	GMT
hello
Link Africa/Abidjan Africa/Bamako	# Mali
Link Africa/Abidjan Africa/Banjul	# Gambia
Link Africa/Abidjan Africa/Conakry	# Guinea
Link Africa/Abidjan Africa/Dakar	# Senegal
Link Africa/Abidjan Africa/Freetown	# Sierra Leone
Link Africa/Abidjan Africa/Lome		# Togo
Link Africa/Abidjan Africa/Nouakchott	# Mauritania
Link Africa/Abidjan Africa/Ouagadougou	# Burkina Faso
Link Africa/Abidjan Atlantic/St_Helena	# St Helena

|]

mixed :: String
mixed = [r|
# http://www.irishstatutebook.ie/eli/1926/sro/919/made/en/print
# http://www.irishstatutebook.ie/eli/1947/sro/71/made/en/print

# Rule	NAME	FROM	TO	TYPE	IN	ON	AT	SAVE	LETTER/S
# Summer Time Act, 1916
Rule	GB-Eire	1916	only	-	May	21	2:00s	1:00	BST
Rule	GB-Eire	1916	only	-	Oct	 1	2:00s	0	GMT
# S.R.&O. 1917, No. 358
Rule	GB-Eire	1917	only	-	Apr	 8	2:00s	1:00	BST
Rule	GB-Eire	1917	only	-	Sep	17	2:00s	0	GMT
# S.R.&O. 1918, No. 274

# Use Europe/London for Jersey, Guernsey, and the Isle of Man.

# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone	Europe/London	-0:01:15 -	LMT	1847 Dec  1  0:00s
			 0:00	GB-Eire	%s	1968 Oct 27
			 1:00	-	BST	1971 Oct 31  2:00u
			 0:00	GB-Eire	%s	1996
			 0:00	EU	GMT/BST

# The following is like GB-Eire and EU, except with standard time in
# summer and negative daylight saving time in winter.  It is for when
# negative SAVE values are used.
# Rule	NAME	FROM	TO	TYPE	IN	ON	AT	SAVE	LETTER/S
Rule	Eire	1971	only	-	Oct	31	 2:00u	-1:00	-
Rule	Eire	1972	1980	-	Mar	Sun>=16	 2:00u	0	-
Rule	Eire	1972	1980	-	Oct	Sun>=23	 2:00u	-1:00	-
Rule	Eire	1981	max	-	Mar	lastSun	 1:00u	0	-
Rule	Eire	1981	1989	-	Oct	Sun>=23	 1:00u	-1:00	-
Rule	Eire	1990	1995	-	Oct	Sun>=22	 1:00u	-1:00	-
Rule	Eire	1996	max	-	Oct	lastSun	 1:00u	-1:00	-

# Côte d'Ivoire / Ivory Coast
# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone	Africa/Abidjan	-0:16:08 -	LMT	1912
			 0:00	-	GMT
Link Africa/Abidjan Africa/Bamako	# Mali
Link Africa/Abidjan Africa/Banjul	# Gambia
Link Africa/Abidjan Africa/Conakry	# Guinea
Link Africa/Abidjan Africa/Dakar	# Senegal
Link Africa/Abidjan Africa/Freetown	# Sierra Leone
Link Africa/Abidjan Africa/Lome		# Togo
Link Africa/Abidjan Africa/Nouakchott	# Mauritania
Link Africa/Abidjan Africa/Ouagadougou	# Burkina Faso
Link Africa/Abidjan Atlantic/St_Helena	# St Helena


Zone	Europe/London	-0:01:15 -	LMT	1847 Dec  1  0:00s
			 0:00	GB-Eire	%s	1968 Oct 27
			 1:00	-	BST	1971 Oct 31  2:00u
			 0:00	GB-Eire	%s	1996
			 0:00	EU	GMT/BST
|]

mixed2 :: String
mixed2 = [r|
# http://www.irishstatutebook.ie/eli/1926/sro/919/made/en/print
# http://www.irishstatutebook.ie/eli/1947/sro/71/made/en/print

# Rule	NAME	FROM	TO	TYPE	IN	ON	AT	SAVE	LETTER/S
# Summer Time Act, 1916
Rule	GB-Eire	1916	only	-	May	21	2:00s	1:00	BST
Rule	GB-Eire	1916	only	-	Oct	 1	2:00s	0	GMT
# S.R.&O. 1917, No. 358
Rule	GB-Eire	1917	only	-	Apr	 8	2:00s	1:00	BST
Rule	GB-Eire	1917	only	-	Sep	17	2:00s	0	GMT
# S.R.&O. 1918, No. 274

# Use Europe/London for Jersey, Guernsey, and the Isle of Man.

# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone	Europe/London	-0:01:15 -	LMT	1847 Dec  1  0:00s
			 0:00	GB-Eire	%s	1968 Oct 27
			 1:00x	-	BST	1971 Oct 31  2:00u
			 0:00	GB-Eire	%s	1996
			 0:00	EU	GMT/BST

# The following is like GB-Eire and EU, except with standard time in
# summer and negative daylight saving time in winter.  It is for when
# negative SAVE values are used.
# Rule	NAME	FROM	TO	TYPE	IN	ON	AT	SAVE	LETTER/S
Rule	Eire	1971	only	-	Oct	31	 2:00u	-1:00	-
Rule	Eire	1972	1980	-	Mar-	Sun>=16	 2:00u	0	-
Rule	Eire	1972	1980	-	Oct	Sun>=23	 2:00u	-1:00	-
Rule	Eire	1981	max	-	Mar	lastSun	 1:00u	0	-
Rule	Eire	1981	1989	-	Oct	Sun>=23	 1:00u	-1:00	-
Rule	Eire	1990	1995	-	Oct	Sun>=22	 1:00u	-1:00	-
Rule	Eire	1996	max	-	Oct	lastSun	 1:00u	-1:00	-

# Côte d'Ivoire / Ivory Coast
# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone	Africa/Abidjan	-0:16:08 -	LMT	1912
			 0:00	-	GMT
Link Africa/Abidjan? Africa/Bamako	# Mali
Link Africa/Abidjan Africa/Banjul	# Gambia
Link Africa/Abidjan Africa/Conakry	# Guinea
Link Africa/Abidjan Africa/Dakar	# Senegal
Link Africa/Abidjan Africa/Freetown	# Sierra Leone
Link Africa/Abidjan Africa/Lome		# Togo
Link Africa/Abidjan Africa/Nouakchott	# Mauritania
Link Africa/Abidjan Africa/Ouagadougou	# Burkina Faso
Link Africa/Abidjan Atlantic/St_Helena	# St Helena


Zone	Europe/London	-0:01:15 -	LMT	1847 Dec  1  0:00s
			 0:00	GB-Eire	%s	1968 Oct 27
			 1:00	-	BST	1971 Oct 31  2:00u
			 0:00	GB-Eire	%s	1996
			 0:00	EU	GMT/BST
|]


parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' r' = parse r' ""

main :: IO ()
main = hspec $ do
  describe "Rules" $ do
    it "parses a 'only' end year" $ do
      parse' pRule_ rule1 `parseSatisfies` (\(_, Rule_{..}) -> toYear == Just 1921)
    it "parses an ending year" $ do
      parse' pRule_ rule2 `parseSatisfies` (\(_, Rule_{..}) -> toYear == Just 1923)
    it "parses a missing ending year" $ do
      parse' pRule_ rule3 `parseSatisfies` (\(_, Rule_{..}) -> toYear == Nothing)

    it "parses at without letter" $ do
      parse' pRule_ rule1 `parseSatisfies` (\(_, Rule_{..}) -> at == At (23 * 60 * 60) 'w')
    it "parses at with a letter" $ do
      parse' pRule_ rule2 `parseSatisfies` (\(_, Rule_{..}) -> at == At (23 * 60 * 60) 's')

    it "parses the save field" $ do
      parse' pRule_ rule1 `parseSatisfies` (\(_, Rule_{..}) -> save == Just (1 * 60 * 60))

    it "parses a numeric day" $ do
      parse' pRule_ rule1 `parseSatisfies` (\(_, Rule_{..}) -> day == Day (Just 14) Nothing Nothing)
    it "parses a week day" $ do
      parse' pRule_ rule2 `parseSatisfies` (\(_, Rule_{..}) -> day == Day Nothing (Just 7) Nothing)
    it "parses lastSun" $ do
      parse' pRule_ rule3 `parseSatisfies` (\(_, Rule_{..}) -> day == Day Nothing (Just 7) (Just Last))
    it "parses day after num" $ do
      parse' pRule_ rule4 `parseSatisfies` (\(_, Rule_{..}) -> day == Day (Just 12) (Just 7) (Just Gte))
    it "parses day before num" $ do
      parse' pRule_ rule5 `parseSatisfies` (\(_, Rule_{..}) -> day == Day (Just 3) (Just 1) (Just Lte))

    it "parses a block of rules" $ do
      parse' pRules_ rules `parseSatisfies` (\v -> length v == 22)

  describe "Zones" $ do
    it "parses the zone name" $ do
      parse' pZoneHead "Zone Atlantic/Cape_Verde" `parseSatisfies` ((==) "Atlantic/Cape_Verde")

    it "parses an until with only a year" $ do
      parse' pUntil "1911" `parseSatisfies` ((==) (Until 1911 1 1 (At 0 'w')))

    it "parses an until without time" $ do
      parse' pUntil "1922 Mar 10" `parseSatisfies` ((==) (Until 1922 3 10 (At 0 'w')))

    it "parses the first zone line" $ do
      parse' pZone zone1 `parseSatisfies` ((==) ((,) "Atlantic/Cape_Verde" [Zone_ (-5644) Nothing Nothing "LMT" (Just $ Until 1912 1 1 (At 7200 'u'))]))

    it "parses a zone with an offset" $ do
      parse' pZone zone2 `parseSatisfies` ((==) ((,) "Atlantic/Cape_Verde" [Zone_ (-5644) Nothing (Just 3600) "LMT" (Just $ Until 1912 1 1 (At 7200 'u'))]))

    it "parses many zones" $ do
      parse' pZone zones `parseSatisfies` (\v -> length (snd v) == 10)

    it "parses cape verde zones" $ do
      parse' pZone zones2 `parseSatisfies` (\v -> length (snd v) == 5)

  describe "Links" $ do
    it "parses some links" $ do
      parse' pAllLinks links `parseSatisfies` (\v -> length v == 9)

  describe "Mixed" $ do
    it "parses multiple rules blocks" $ do
      parse' pAllRules_ mixed `parseSatisfies` (\v -> length v == 11)

    it "fails parsing multiple rules with an error" $ do
      parse' pAllRules_ `shouldFailOn` mixed2

    it "parses multiple zones blocks" $ do
      parse' pAllZones mixed `parseSatisfies` (\v -> length v == 3)

    it "fails parsing multiple zones with an error" $ do
      parse' pAllZones `shouldFailOn` mixed2

    it "parses multiple links" $ do
      parse' pAllLinks mixed `parseSatisfies` (\v -> length v == 9)

    it "fails parsing multiple links with an error" $ do
      parse' pAllLinks `shouldFailOn` mixed2

  -- describe "encodes" $ do
  --   it "parse some stuff" $ do
  --     let r = parse' pAllRules_ mixed
  --         z = parse' pAllZones mixed
  --         l = parse' pAllLinks mixed
  --     case (r, z, l) of
  --       (Right r', Right z', Right l') ->
  --         encode (encodeRegion r' z' l') `shouldBe` ""
  --       (_, _, _) ->
  --         error "Error"
