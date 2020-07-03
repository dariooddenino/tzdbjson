{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.RawString.QQ
import           Tzdbjson

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

zones = [r|# more precise 0:09:21.
# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone	Africa/Algiers	0:12:12 -	LMT	1891 Mar 15  0:01
			0:09:21	-	PMT	1911 Mar 11 # Paris Mean Time
			0:00	Algeria	WE%sT	1940 Feb 25  2:00
			1:00	Algeria	CE%sT	1946 Oct  7
			0:00	-	WET	1956 Jan 29
			1:00	-	CET	1963 Apr 14
			0:00	Algeria	WE%sT	1977 Oct 21
			1:00	Algeria	CE%sT	1979 Oct 26
			0:00	Algeria	WE%sT	1981 May
			1:00	-	CET

# Angola
|]

main :: IO ()
main = do
  parseTest (many (pRule)) rules
  pure ()
