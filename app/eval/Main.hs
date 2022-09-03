module Main where

import Codec.Picture
import Options.Applicative

import EvalJuicyPixels
import Types


data Options
  = Options
  { optOutput :: Maybe FilePath
  , optTarget :: Maybe FilePath
  , optInput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> outputOption
  <*> targetOption
  <*> fileInput
  where
    fileInput :: Parser FilePath
    fileInput = argument str (metavar "FILE")

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

  (targetImage, size) <-
    case optTarget opt of
      Nothing -> do
        -- TODO: サイズは別途指定できるように
        return (Nothing, (400, 400))
      Just fname -> do
        Right (ImageRGBA8 img) <- readImage fname
        return (Just img, (imageWidth img, imageHeight img))

  moves <- loadISL (optInput opt)
  let (img, cost) = evalISLWithCost size moves

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
