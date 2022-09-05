{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Types where

import GHC.Generics (Generic)
import Codec.Picture
import Control.Exception (assert)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, genericParseJSON, genericToJSON, defaultOptions, Options (..))
import qualified Data.Aeson as JSON
import Data.Char
import qualified Data.Map as Map
import Data.List
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP

type Color = RGBA
type RGBA = (Int, Int, Int, Int)

data Shape
    = Rectangle
        { leftBottom :: Point
        , rightUpper :: Point
        }
    deriving (Eq, Show)

shapeWidth :: Shape -> Int
shapeWidth (Rectangle (x0, _y0) (x1, _y1)) = x1 - x0

shapeHeight :: Shape -> Int
shapeHeight (Rectangle (_x0, y0) (_x1, y1)) = y1 - y0

shapeSize :: Shape -> Int
shapeSize s = shapeWidth s * shapeHeight s

sameShape :: Shape -> Shape -> Bool
sameShape (Rectangle (x00, y00) (x01, y01)) (Rectangle (x10, y10) (x11, y11))
    = x01 - x00 == x11 - x10 && y01 -y00 == y11 - y10

-- |
-- >>> compatibleShape (Rectangle (0,0) (1,1)) (Rectangle (1,0) (2,1))
-- True
-- >>> compatibleShape (Rectangle (0,0) (1,1)) (Rectangle (1,1) (2,2))
-- False
-- >>> compatibleShape (Rectangle (0,0) (1,1)) (Rectangle (0,1) (1,2))
-- True
-- >>> compatibleShape (Rectangle (0,0) (1,1)) (Rectangle (-1,0) (0,1))
-- True
-- >>> compatibleShape (Rectangle (0,0) (1,1)) (Rectangle (0,-1) (1,0))
-- True
compatibleShape :: Shape -> Shape -> Bool
compatibleShape (Rectangle (x00, y00) (x01, y01)) (Rectangle (x10, y10) (x11, y11))
    = or $ map and [ [ x00 == x10, y01 == y10, x01 == x11 ]
                   , [ x00 == x10, y00 == y11, x01 == x11 ]
                   , [ x01 == x10, y00 == y10, y01 == y11 ]
                   , [ x00 == x11, y00 == y10, y01 == y11 ]
                   ]

intersectShape :: Shape -> Shape -> Maybe Shape
intersectShape (Rectangle (x00, y00) (x01, y01)) (Rectangle (x10, y10) (x11, y11)) = if
    | x00 >= x11 || x01 <= x10 -> Nothing
    | y00 >= y11 || y01 <= y10 -> Nothing
    | otherwise -> Just $ Rectangle (max x00 x10, max y00 y10) (min x01 x11, min y01 y11)

-- |
-- >>> mergeShape (Rectangle (0,0) (1,1)) (Rectangle (1,0) (2,1))
-- Just (Rectangle {leftBottom = (0,0), rightUpper = (2,1)})
mergeShape :: Shape -> Shape -> Maybe Shape
mergeShape s1@(Rectangle (x00, y00) (x01, y01)) s2@(Rectangle (x10, y10) (x11, y11)) = do
    guard $ compatibleShape s1 s2
    return $ Rectangle (min x00 x10, min y00 y10) (max x01 x11, max y01 y11)

type BlockId = V.Vector Int

instance {-# Overlapping #-} Read BlockId where
    readsPrec _ = readP_to_S rBlockId

rBlockId :: ReadP BlockId
rBlockId = rBracket (V.fromList <$> sepBy1 rInt (char '.'))

rInt :: ReadP Int
rInt = read <$> many1 (satisfy isDigit)

data Orientation
    = X
    | Y
    deriving (Eq, Show, Read)

rOrientation :: ReadP Orientation
rOrientation = rBracket $ ((char 'X' +++ char 'x') *> pure X) +++ ((char 'Y' +++ char 'y') *> pure Y)

type Offset = Int
type Point = (Int, Int)

data Move
    = LCUT  BlockId Orientation Offset
    | PCUT  BlockId Point
    | COLOR BlockId Color
    | SWAP  BlockId BlockId
    | MERGE BlockId BlockId
    deriving Eq

instance Show Move where
    show = dispMove

instance Read Move where
    readsPrec _ = readP_to_S (skipSpaces *> rMove)

rMove :: ReadP Move
rMove = rLCutMove +++ rPCutMove +++ rColorMove +++ rSwapMove +++ rMergeMove

rLCutMove :: ReadP Move
rLCutMove = LCUT <$> (string "cut" *> skipSpaces *> rBlockId) <*> rOrientation <*> rOffset

rBracket :: ReadP a -> ReadP a
rBracket p = between (char '[' <* skipSpaces) (char ']') p <* skipSpaces

rOffset :: ReadP Offset
rOffset = rBracket (readS_to_P reads)

rPCutMove :: ReadP Move
rPCutMove = PCUT <$> (string "cut" *> skipSpaces *> rBlockId) <*> rPoint

rPoint :: ReadP Point
rPoint = rBracket ((,) <$> rInt <* char ',' <* skipSpaces <*> rInt)

rColorMove :: ReadP Move
rColorMove = COLOR <$> (string "color" *> skipSpaces *> rBlockId) <*> rColor

rColor :: ReadP Color
rColor = rBracket ((,,,) <$> rInt <* (char ',' <* skipSpaces) <*> rInt <* (char ',' <* skipSpaces) <*> rInt <* (char ',' <* skipSpaces) <*> rInt)

rSwapMove :: ReadP Move
rSwapMove = SWAP <$> (string "swap" *> skipSpaces *> rBlockId) <*> rBlockId

rMergeMove :: ReadP Move
rMergeMove = MERGE <$> (string "merge" *> skipSpaces *> rBlockId) <*> rBlockId

dispMove :: Move -> String
dispMove = \case
    LCUT bid ori off   -> intercalate " " [ "cut"
                                          , dispBlockId bid
                                          , dispOrientation ori
                                          , dispOffset off]
    PCUT bid pnt       -> intercalate " " [ "cut"
                                          , dispBlockId bid
                                          , dispPoint pnt]
    COLOR bid color    -> intercalate " " [ "color"
                                          , dispBlockId bid
                                          , dispColor color ]
    SWAP bid0 bid1     -> intercalate " " [ "swap"
                                          , dispBlockId bid0
                                          , dispBlockId bid1
                                          ]
    MERGE bid0 bid1    -> intercalate " " [ "merge"
                                          , dispBlockId bid0
                                          , dispBlockId bid1
                                          ]

dispBlockId :: BlockId -> String
dispBlockId = dispBetween "[" "]" . intercalate "." . map show . V.toList

dispBetween :: String -> String -> String -> String
dispBetween o c s = o ++ s ++ c

dispOrientation :: Orientation -> String
dispOrientation = dispBetween "[" "]" . show

dispOffset :: Offset -> String
dispOffset = dispBetween "[" "]" . show

dispPoint :: Point -> String
dispPoint (x,y) = dispBetween "[" "]"
                  (intercalate "," (map show [x,y]))

dispColor :: Color -> String
dispColor (r,g,b,a) = dispBetween "[" "]"
                      (intercalate "," (map show [r,g,b,a]))


loadISL :: FilePath -> IO [Move]
loadISL fname = do
  s <- readFile fname
  return [read l | l <- lines s, not ("#" `isPrefixOf` l)]

-- ------------------------------------------------------------------------

data InitialConfig
  = InitialConfig
  { icWidth :: !Int
  , icHeight :: !Int
  , icSourcePngJSON :: Maybe String
  , icSourcePngPNG ::Maybe String
  , icBlocks :: [ICBlock]
  }
  deriving (Show, Generic)

initialConfigAesonOptions :: Options
initialConfigAesonOptions = defaultOptions{fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 2}

instance FromJSON InitialConfig where
  parseJSON = genericParseJSON initialConfigAesonOptions

instance ToJSON InitialConfig where
  toJSON = genericToJSON initialConfigAesonOptions

icCounter :: InitialConfig -> Int
icCounter config = maximum [V.head (icbBlockIdParsed block) | block <- icBlocks config]

data ICBlock
  = ICBlock
  { icbBlockId :: String
  , icbBottomLeft :: Point
  , icbTopRight :: Point
  , icbColor :: Maybe Color
  , icbPngBottomLeftPoint :: Maybe Point
  }
  deriving (Show, Generic)

icBlockAesonOptions :: Options
icBlockAesonOptions = defaultOptions{fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 3}

instance FromJSON ICBlock where
  parseJSON = genericParseJSON icBlockAesonOptions

instance ToJSON ICBlock where
  toJSON = genericToJSON icBlockAesonOptions

icbParseBlockId :: String -> BlockId
icbParseBlockId s = read ("[" ++ s ++ "]")

icbBlockIdParsed :: ICBlock -> BlockId
icbBlockIdParsed = icbParseBlockId . icbBlockId

icbShape :: ICBlock -> Shape
icbShape block = Rectangle (icbBottomLeft block) (icbTopRight block)

loadInitialConfig :: FilePath -> IO InitialConfig
loadInitialConfig fname = do
  ret <- JSON.eitherDecodeFileStrict' fname
  case ret of
    Right ic -> return ic
    Left em  -> fail $ "fail to parse initial configuration: " ++ em

defaultInitialConfig :: InitialConfig
defaultInitialConfig
  = InitialConfig
  { icWidth = 400
  , icHeight = 400
  , icSourcePngJSON = Nothing
  , icSourcePngPNG = Nothing
  , icBlocks =
      [ ICBlock
        { icbBlockId = "0"
        , icbBottomLeft = (0, 0)
        , icbTopRight = (400, 400)
        , icbColor = Just (255, 255, 255, 255)
        , icbPngBottomLeftPoint = Nothing
        }
      ]
  }

-- ------------------------------------------------------------------------

similarity :: Image PixelRGBA8 -> Image PixelRGBA8 -> Integer
similarity img1 img2 = assert (imageWidth img1 == imageWidth img2 && imageHeight img1 == imageHeight img2) $
  roundJS $ (alpha *) $ sum $
    [ pixelDiff p1 p2
    | y <- [0 .. imageHeight img1 - 1]
    , x <- [0 .. imageWidth img1 - 1]
    , let p1 = pixelAt img1 x y, let p2 = pixelAt img2 x y
    ]
  where
    alpha = 0.005


pixelDiff :: PixelRGBA8 -> PixelRGBA8 -> Double
pixelDiff (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  sqrt $ sum
    [ (fromIntegral r1 - fromIntegral r2)^(2::Int)
    , (fromIntegral g1 - fromIntegral g2)^(2::Int)
    , (fromIntegral b1 - fromIntegral b2)^(2::Int)
    , (fromIntegral a1 - fromIntegral a2)^(2::Int)
    ]


-- Haskell の round は偶数丸めだが、JavascriptのMath.roundは四捨五入なので、それに合わせる
-- https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/round
roundJS :: (RealFrac a, Integral b) => a -> b
roundJS x = floor (x + 0.5)

-- ------------------------------------------------------------------------

sampleMoves =
  [ COLOR (V.fromList [0]) (0,74,173,255)
  , PCUT (V.fromList [0]) (360,40)
  , PCUT (V.fromList [0,3]) (320,80)
  , COLOR (V.fromList [0,3,3]) (0,0,0,255)
  , PCUT (V.fromList [0,3,3]) (160,240)
  --
  , PCUT (V.fromList [0,3,3,0]) (80,160)
  , PCUT (V.fromList [0,3,3,0,0]) (40,120)
  , COLOR (V.fromList [0,3,3,0,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,0,1]) (120,120)
  , COLOR (V.fromList [0,3,3,0,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,0,2]) (120,200)
  , COLOR (V.fromList [0,3,3,0,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,0,3]) (40,200)
  , COLOR (V.fromList [0,3,3,0,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,3,3]) (255,255,255,255)
  --
  , PCUT (V.fromList [0,3,3,1]) (240,160)
  , PCUT (V.fromList [0,3,3,1,0]) (200,120)
  , COLOR (V.fromList [0,3,3,1,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,1,1]) (280,120)
  , COLOR (V.fromList [0,3,3,1,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,1,2]) (280,200)
  , COLOR (V.fromList [0,3,3,1,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,1,3]) (200,200)
  , COLOR (V.fromList [0,3,3,1,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,3,3]) (255,255,255,255)
  --
  , PCUT (V.fromList [0,3,3,2]) (240,320)
  , PCUT (V.fromList [0,3,3,2,0]) (200,280)
  , COLOR (V.fromList [0,3,3,2,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,2,1]) (280,280)
  , COLOR (V.fromList [0,3,3,2,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,2,2]) (280,360)
  , COLOR (V.fromList [0,3,3,2,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,2,3]) (200,360)
  , COLOR (V.fromList [0,3,3,2,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,3,3]) (255,255,255,255)
  --
  , PCUT (V.fromList [0,3,3,3]) (80,320)
  , PCUT (V.fromList [0,3,3,3,0]) (40,280)
  , COLOR (V.fromList [0,3,3,3,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,3,1]) (120,280)
  , COLOR (V.fromList [0,3,3,3,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,3,2]) (120,360)
  , COLOR (V.fromList [0,3,3,3,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,3,3]) (40,360)
  , COLOR (V.fromList [0,3,3,3,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,3,3]) (255,255,255,255)
  --
  , COLOR (V.fromList [0,3,0]) (0,0,0,255)
  , LCUT (V.fromList [0,3,0]) X 160
  , LCUT (V.fromList [0,3,0,0]) X 80
  , LCUT (V.fromList [0,3,0,0,0]) X 40
  , COLOR (V.fromList [0,3,0,0,0,0]) (255,255,255,255)
  , LCUT (V.fromList [0,3,0,0,1]) X 120
  , COLOR (V.fromList [0,3,0,0,1,0]) (255,255,255,255)
  , LCUT (V.fromList [0,3,0,1]) X 240
  , LCUT (V.fromList [0,3,0,1,0]) X 200
  , COLOR (V.fromList [0,3,0,1,0,0]) (255,255,255,255)
  , LCUT (V.fromList [0,3,0,1,1]) X 280
  , COLOR (V.fromList [0,3,0,1,1,0]) (255,255,255,255)
  --
  , COLOR (V.fromList [0,3,2]) (0,0,0,255)
  , LCUT (V.fromList [0,3,2]) Y 240
  , LCUT (V.fromList [0,3,2,0]) Y 160
  , LCUT (V.fromList [0,3,2,0,0]) Y 120
  , COLOR (V.fromList [0,3,2,0,0,1]) (255,255,255,255)
  , LCUT (V.fromList [0,3,2,0,1]) Y 200
  , COLOR (V.fromList [0,3,2,0,1,1]) (255,255,255,255)
  , LCUT (V.fromList [0,3,2,1]) Y 320
  , LCUT (V.fromList [0,3,2,1,0]) Y 280
  , COLOR (V.fromList [0,3,2,1,0,1]) (255,255,255,255)
  , LCUT (V.fromList [0,3,2,1,1]) Y 360
  , COLOR (V.fromList [0,3,2,1,1,1]) (255,255,255,255)
  ]
