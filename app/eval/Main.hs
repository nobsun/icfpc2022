module Main where

import Codec.Picture
import Control.Monad
import Options.Applicative

import qualified EvalContent
import qualified EvalJuicyPixels
import Types


data Options
  = Options
  { optOutput :: Maybe FilePath
  , optInitialConfig :: Maybe FilePath
  , optSourceImage :: Maybe FilePath
  , optTarget :: Maybe FilePath
  , optEvalEngine :: String
  , optInput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> outputOption
  <*> initialConfigOption
  <*> sourceImageOption
  <*> targetOption
  <*> evalEngineOption
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

    sourceImageOption :: Parser (Maybe FilePath)
    sourceImageOption = optional $ strOption
      $  short 's'
      <> metavar "FILE"
      <> help "source image filename"
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

    evalEngineOption :: Parser String
    evalEngineOption = strOption
      $  short 'e'
      <> metavar "ENGINE"
      <> help "evaluation engine name: \"content\" or default engine"
      <> value ""
      <> showDefault


parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "eval - ISL evaluator"


main :: IO ()
main = do
  opt <- execParser parserInfo

  let eval =
        if optEvalEngine opt == "content"
        then EvalContent.evalISLWithCost
        else EvalJuicyPixels.evalISLWithCost

  initialConfig <-
    case optInitialConfig opt of
      Nothing -> return defaultInitialConfig
      Just fname -> loadInitialConfig fname

  sourceImage <-
    case optSourceImage opt of
      Nothing -> return Nothing
      Just fname -> do
        Right (ImageRGBA8 img) <- readImage fname
        return (Just img)

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
    case eval initialConfig sourceImage moves of
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
