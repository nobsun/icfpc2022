module Main where

import Codec.Picture
import Codec.Picture.Types (freezeImage)
import Control.Monad
import Control.Monad.ST
import Data.List
import Options.Applicative
import qualified Data.Vector.Unboxed.Mutable as VUM

import EvalJuicyPixels (initializeImage)
import Types


data Options
  = Options
  { optInitialConfig :: Maybe FilePath
  , optSourceImage :: Maybe FilePath
  , optInput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> initialConfigOption
  <*> sourceImageOption
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

  sourceImage <-
    case optSourceImage opt of
      Nothing -> return Nothing
      Just fname -> do
        Right (ImageRGBA8 img) <- readImage fname
        return (Just img)
  img0 <- freezeImage =<< initializeImage config sourceImage

  forM_ (icBlocks config) $ \block -> do
    let bid = icbBlockIdParsed block
        shape = Rectangle (icbBottomLeft block) (icbTopRight block)
        (x1, y1) = icbBottomLeft block
        (x2, y2) = icbTopRight block
        px@(PixelRGBA8 r g b a) = averageColor img shape

        move = COLOR bid (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)

        sim1, sim2 :: Double
        sim1 = sum [ pixelDiff px0 px2
                   | y <- [y1..y2-1], x <- [x1..x2-1]
                   , let px0 = pixelAt img0 x (imageHeight img0 - 1 - y)
                   , let px2 = pixelAt img x (imageHeight img - 1 - y)
                   ]
        sim2 = sum [pixelDiff px px2 | y <- [y1..y2-1], x <- [x1..x2-1], let px2 = pixelAt img x (imageHeight img - 1 - y)]

        cost :: Integer
        cost = roundJS (baseCost config move * fromIntegral (icWidth config * icHeight config) / fromIntegral (shapeSize shape) :: Double)

    when (fromIntegral cost <= alpha * (sim1 - sim2)) $
      putStrLn $ dispMove move

alpha = 0.005

