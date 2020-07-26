module Main (main) where

-- import Tzdbjson
-- argument str (metavar "FILE")

import Options.Applicative

data Program = Program
  { file :: String }

program :: Parser Program
program = Program
  <$> argument str (metavar "FILE")

main :: IO ()
main = pure ()
