module Main (main) where

import qualified Data.ByteString.Lazy   as B
import qualified Data.Either.Validation as V
import           Options.Applicative
import           Tzdbjson
import           Tzdbjson.Types

data Program
  = FileInput FilePath (Maybe FilePath)
  | DirectoryInput FilePath(Maybe FilePath)

fileInput :: Parser Program
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Convert a single tzdb file"
  ) <*> optional (strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "The output file"
  ))

directoryInput :: Parser Program
directoryInput = DirectoryInput <$> strOption
  (  long "directory"
  <> short 'd'
  <> metavar "DIRECTORY"
  <> help "Convert all of the tzdb files inside of the directory"
  ) <*> optional (strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "The output file"
  ))

main :: IO ()
main = run =<< execParser opts
  where
    opts = info ((fileInput <|> directoryInput) <**> helper)
      ( fullDesc
      <> progDesc "Convert a single file or the contents of a directory to JSON"
      <> header "A CLI app to convert tzdb files to JSON")

run :: Program -> IO ()
run (FileInput f o) = do
  result <- encodeFile f
  case result of
    V.Failure errors ->
      printErrors f errors
    V.Success v -> do
      case o of
        Nothing -> B.putStr v
        Just o' -> B.writeFile o' v
run (DirectoryInput d o) = do
  -- scan the directory and run encodeFile on each
  -- if the directory is missing, write in the same path, appending json
  -- otherwise use the passed directory
  pure ()


-- run (Program f) = do
--   result <- encodeFile f
--   case result of
--     V.Failure errors ->
--       putStrLn "We got errors"
--     V.Success v ->
--       B.putStr v
