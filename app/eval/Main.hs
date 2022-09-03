module Main where

import Codec.Picture
import Control.Monad
import Options.Applicative

import EvalJuicyPixels
import Types


data Options
  = Options
  { optOutput :: Maybe FilePath
  , optInitialConfig :: Maybe FilePath
  , optTarget :: Maybe FilePath
  , optInput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> outputOption
  <*> initialConfigOption
  <*> targetOption
  <*> fileInput
  where
    fileInput :: Parser FilePath
    fileInput = argument str (metavar "FILE")

    initialConfigOption :: Parser (Maybe FilePath)
    initialConfigOption = optional $ strOption
      $  short 'c'
      <> metavar "FILE"
      <> help "initial config"
      <> showDefaultWith id

    outputOption :: Parser (Maybe FilePath)
    outputOption = optional $ strOption
      $  short 'o'
      <> metavar "FILE"
      <> help "output image filename"
      <> showDefaultWith id

    targetOption :: Parser (Maybe FilePath)
    targetOption = optional $ strOption
      $  short 't'
      <> metavar "FILE"
      <> help "target image filename"
      <> showDefaultWith id


parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "eval - ISL evaluator"


main :: IO ()
main = do
  opt <- execParser parserInfo

  initialConfig <-
    case optInitialConfig opt of
      Nothing -> return defaultInitialConfig
      Just fname -> loadInitialConfig fname

  targetImage <-
    case optTarget opt of
      Nothing -> do
        return Nothing
      Just fname -> do
        Right (ImageRGBA8 img) <- readImage fname
        unless (icWidth initialConfig == imageWidth img && icHeight initialConfig == imageHeight img) $
          fail "size mismatch"
        return (Just img)

  moves <- loadISL (optInput opt)
  (img, cost) <-
    case evalISLWithCost initialConfig moves of
      Left err -> fail err
      Right (img, cost) -> return (img, cost)

  putStrLn $ "ISL code cost: " ++ show cost
  sim <-
    case targetImage of
      Nothing -> return 0
      Just img2 -> do
        let sim = similarity img img2
        putStrLn $ "Similarity: " ++ show sim
        return sim
  putStrLn $ "Total cost: " ++ show (cost + sim)

  case optOutput opt of
    Nothing -> return ()
    Just fname -> writePng fname img
