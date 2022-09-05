{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Codec.Picture
import Data.List
import Options.Applicative
import System.Environment
import Types (ICBlock (..), InitialConfig (..), defaultInitialConfig, loadInitialConfig)
import Control.Monad (forM_)

averageColorOfBlock :: ICBlock -> Image PixelRGBA8 -> PixelRGBA8
averageColorOfBlock ICBlock{icbBottomLeft, icbTopRight} img = p
 where
  p =
    (\(r, g, b, a) -> PixelRGBA8 (round (r / n)) (round (g / n)) (round (b / n)) (round (a / n))) $
      foldl'
        (\(r1, g1, b1, a1) (r2, g2, b2, a2) -> ((((,,,) $! (r1 + r2)) $! (g1 + g2)) $! (b1 + b2)) $! (a1 + a2))
        (0 :: Double, 0 :: Double, 0 :: Double, 0 :: Double)
        [ (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
        | y <- [bot .. top - 1]
        , x <- [left .. right - 1]
        , let PixelRGBA8 r g b a = pixelAt img x y
        ]
  n = fromIntegral (imageHeight img) * fromIntegral (imageWidth img)
  (bot, left) = icbBottomLeft
  (top, right) = icbTopRight

data Options = Options
  { optInput :: FilePath
  , optInitialConfig :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> fileInput
    <*> initialConfigOption
 where
  fileInput :: Parser FilePath
  fileInput = argument str (metavar "FILE")

  initialConfigOption :: Parser (Maybe FilePath)
  initialConfigOption =
    optional $
      strOption $
        short 'c'
          <> metavar "FILE"
          <> help "initial config"
          <> showDefaultWith id

parserInfo :: ParserInfo Options
parserInfo =
  info (optionsParser <**> helper) $
    fullDesc
      <> header "eval - ISL evaluator"

main :: IO ()
main = do
  Options{..} <- execParser parserInfo
  initialConfig <-
    case optInitialConfig of
      Nothing -> return defaultInitialConfig
      Just fname -> loadInitialConfig fname
  Right (ImageRGBA8 img) <- readImage optInput
  forM_ (icBlocks initialConfig) $ \block -> do
    let PixelRGBA8 r g b a = averageColorOfBlock block img
    putStrLn $ "color [" ++ icbBlockId block ++ "] " ++ show [r, g, b, a]
