{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Types where

import Data.Char
import qualified Data.Map as Map
import Data.List
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

type Id = Int

-- Note that numbers are reversed
type BlockId = [Int]

instance {-# Overlapping #-} Read BlockId where
    readsPrec _ = readP_to_S rBlockId

rBlockId :: ReadP BlockId
rBlockId = rBracket (reverse <$> sepBy1 rInt (char '.'))

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
    Move mv     -> case mv of
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
        SWAP bid1 bid2     -> intercalate " " [ "swap"
                                              , dispBlockId bid1
                                              , dispBlockId bid2
                                              ]
        MERGE bid1 bid2    -> intercalate " " [ "merge"
                                              , dispBlockId bid1
                                              , dispBlockId bid2
                                              ]

dispBlockId :: BlockId -> String
dispBlockId = dispBetween "[" "]" . intercalate "." . map show . reverse

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
    , costs       :: [Int]
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
    , blocks = Map.singleton [0] 
                 (SimpleBlock (Rectangle (0,0) (400, 400)) white)
    , pict   = undefined
    , costs  = [0]
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
