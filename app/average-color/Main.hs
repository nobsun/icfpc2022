module Main where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.List
import Options.Applicative
import qualified Data.Vector.Unboxed.Mutable as VUM

import Types


data Options
  = Options
  { optInitialConfig :: Maybe FilePath
  , optInput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> initialConfigOption
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


parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "average-color - simplest solver"


averageColor :: Image PixelRGBA8 -> Shape -> PixelRGBA8
averageColor img (Rectangle (x1,y1) (x2,y2)) = runST $ do
  vec <- VUM.replicate 4 (0.0 :: Double)
  forM_ [y1..y2-1] $ \y -> do
    forM_ [x1..x2-1] $ \x -> do
      let PixelRGBA8 r g b a = pixelAt img x (imageHeight img - 1 - y)
      VUM.modify vec (+ fromIntegral r) 0
      VUM.modify vec (+ fromIntegral g) 1
      VUM.modify vec (+ fromIntegral b) 2
      VUM.modify vec (+ fromIntegral a) 3
  rSum <- VUM.read vec 0
  gSum <- VUM.read vec 1
  bSum <- VUM.read vec 2
  aSum <- VUM.read vec 3
  let n = fromIntegral ((y2 - y1) * (x2 - x1))
      f x = round (x / n)
  return (PixelRGBA8 (f rSum) (f gSum) (f bSum) (f aSum))


main :: IO ()
main = do
  opt <- execParser parserInfo

  Right (ImageRGBA8 img) <- readImage (optInput opt)

  config <-
    case optInitialConfig opt of
      Nothing -> return defaultInitialConfig
      Just fname -> loadInitialConfig fname

  forM_ (icBlocks config) $ \block -> do
    let bid = icbBlockIdParsed block
        PixelRGBA8 r g b a = averageColor img (Rectangle (icbBottomLeft block) (icbTopRight block))
    putStrLn $ dispMove $ COLOR bid (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
