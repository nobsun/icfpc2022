{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Data.Char
import qualified Data.Map as Map
import Data.List
import qualified Data.Vector as V
-- import qualified Data.Set as Set
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Bitmap as Gloss
import Numeric
import Text.ParserCombinators.ReadP

type BitmapData = Gloss.BitmapData
type Canvas = Shape

type Color = RGBA
type RGBA = (Int, Int, Int, Int)

-- type Block        = Either SimpleBlock ComplexBlock

data Block 
    = SimpleBlock { shape :: Shape, blockColor :: Color }
    | ComplexBlock { shape :: Shape, children :: ChildBlocks }
    deriving (Eq, Show)

type ChildBlocks  = [BlockId]

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
sameShape _ _ = False

compatibleShape :: Shape -> Shape -> Bool
compatibleShape (Rectangle (x00, y00) (x01, y01)) (Rectangle (x10, y10) (x11, y11))
    = or $ map and [ [ x00 == x10, y01 == y10, x01 == x11 ]
                   , [ x00 == x10, y00 == y11, x01 == x11 ]
                   , [ x01 == x10, y00 == y10, y01 == y11 ]
                   , [ x00 == x11, y00 == y10, y01 == y11 ]
                   ]
               
compatibleShape _ _ = False

type Id = Int

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
    deriving (Eq, Show)

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

-- ProgLine

data ProgLine
    = Move Move
    | Newline
    | Comment String
    deriving (Eq, Show)

displayProgLine :: ProgLine -> String
displayProgLine = \ case
    Newline     -> "\n"
    Comment msg -> "# " ++ msg
    Move mv     -> dispMove mv

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

dispBlockEntry :: Map.Map BlockId Block -> (BlockId, Block) -> String
dispBlockEntry tbl (bid, b) = dispBlockId bid ++ ": " ++ dispBlock tbl b

dispBlock :: Map.Map BlockId Block -> Block -> String
dispBlock tbl = \ case
    SimpleBlock  shp col -> show shp ++ " " ++ dispColor col
    ComplexBlock shp bs  -> show shp ++ " [" ++ intercalate ", " (map (dispBlock tbl . (tbl Map.!)) bs) ++ "]"

-- World

type BlockTable = Map.Map BlockId Block

data World 
    = World
    { canvas      :: Shape
    , prog        :: [Instruction]
    , counter     :: Int
    , blocks      :: BlockTable 
    , pict        :: Gloss.Picture
    , costs       :: Int
    }

instance Show World where
    show w = case w of
        World { blocks = tbl }
            -> unlines (map (dispBlockEntry tbl) (Map.assocs tbl))

initializeWorld :: Canvas -> [Instruction] -> World
initializeWorld cvs is
    = World
    { canvas = cvs
    , prog = is
    , counter = 0
    , blocks = Map.singleton (V.singleton 0)
                 (SimpleBlock (Rectangle (0,0) (400, 400)) white)
    , pict   = undefined
    , costs  = 0
    }

initialWorld :: World
initialWorld = initializeWorld (Rectangle (0,0) (399,399)) []


white :: Color
white = (255,255,255,255)
red, green, blue :: Color
red  = (255,0,0,255)
green = (0,255,0,255)
blue  = (0,0,255,255)

incCount :: World -> (Int, World)
incCount world = (cnt, world { counter = succ cnt })
    where
        cnt = counter world

type Instruction = World -> World



loadISL :: FilePath -> IO [Move]
loadISL fname = do
  s <- readFile fname
  return [read l | l <- lines s, not ("#" `isPrefixOf` l)]



data InitialConfig
  = InitialConfig
  { icWidth :: !Int
  , icHeight :: !Int
  , icBlocks :: [ICBlock]
  }
  deriving (Show)

data ICBlock
  = ICBlock
  { icbBlockId :: String
  , icbBottomLeft :: Point
  , icbTopRight :: Point
  , icbColor :: Color
  }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 2} ''InitialConfig)

$(deriveJSON defaultOptions{fieldLabelModifier = (\(c:cs) -> toLower c : cs) . drop 3} ''ICBlock)

loadInitialConfig :: FilePath -> IO InitialConfig
loadInitialConfig fname = do
  ret <- JSON.decodeFileStrict' fname
  case ret of
    Just ic -> return ic
    Nothing -> fail "fail to parse initial configuration"

defaultInitialConfig :: InitialConfig
defaultInitialConfig
  = InitialConfig
  { icWidth = 400
  , icHeight = 400
  , icBlocks =
      [ ICBlock
        { icbBlockId = "0"
        , icbBottomLeft = (0, 0)
        , icbTopRight = (400, 400)
        , icbColor = (255, 255, 255, 255)
        }
      ]
  }
