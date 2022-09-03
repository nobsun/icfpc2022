module Main where

import Codec.Picture
import Control.Exception
import Control.Monad
import Options.Applicative

import EvalJuicyPixels
import Types


data Options
  = Options
  { optOutput :: Maybe FilePath
  , optInput1 :: FilePath
  , optInput2 :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> outputOption
  <*> fileInput
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

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "compare-image"


main :: IO ()
main = do
  opt <- execParser parserInfo

  Right (ImageRGBA8 img1) <- readImage (optInput1 opt)
  Right (ImageRGBA8 img2) <- readImage (optInput2 opt)

  unless ((imageWidth img1, imageHeight img1) == (imageWidth img2, imageHeight img2)) $ do
    fail "image size mismatch"

  let sim = similarity img1 img2
  putStrLn $ "Similarity: " ++ show sim

  case optOutput opt of
    Nothing -> return ()
    Just fname -> do
      let f x y = 255 - round (255 * pixelDiff p1 p2 / sqrt (255^(2::Int) * 4))
            where
              p1 = pixelAt img1 x y
              p2 = pixelAt img2 x y
          diff :: Image Pixel8
          diff = generateImage f (imageWidth img1) (imageHeight img1)
      writePng fname diff
