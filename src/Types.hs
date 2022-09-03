{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Types where

import qualified Data.Map as Map
import Data.List
import qualified Data.Set as Set
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Bitmap as Gloss
import Numeric
import Text.ParserCombinators.ReadP

type BitmapData = Gloss.BitmapData
type Canvas = BitmapData

type Color = RGBA
type RGBA = (Int, Int, Int, Int)

-- type Block        = Either SimpleBlock ComplexBlock

data Block 
    = SimpleBlock { shape :: Shape, blockColor :: Color }
    | ComplexBlock { shape :: Shape, children :: ChildBlocks }
    deriving (Eq, Show)

type ChildBlocks  = Set.Set BlockId

data Shape
    = Rectangle
        { leftBottom :: Point
        , rightUpper :: Point
        }
    deriving (Eq, Show)

type Id = Int
type BlockId = [Int]

instance {-# Overlapping #-} Read BlockId where
    readsPrec _ = readP_to_S rBlockId

rBlockId :: ReadP BlockId
rBlockId = reverse <$> sepBy1 rInt (char '.')

rInt :: ReadP Int
rInt = readS_to_P reads

data Orientation
    = X
    | Y
    deriving (Eq, Show, Read)

rOrientation :: ReadP Orientation
rOrientation = readS_to_P reads

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
    readsPrec _ = readP_to_S rMove

rMove :: ReadP Move
rMove = rLCutMove +++ rPCutMove +++ rColorMove +++ rSwapMove +++ rMergeMove

rLCutMove :: ReadP Move
rLCutMove = LCUT <$> rBlockId <*> rOrientation <*> rOffset

rBracket :: ReadP a -> ReadP a
rBracket = between (char '[') (char ']')

rOffset :: ReadP Offset
rOffset = between (char '[') (char ']') (readS_to_P reads)

rPCutMove :: ReadP Move
rPCutMove = PCUT <$> rBlockId <*> rPoint

rPoint :: ReadP Point
rPoint = rBracket ((,) <$> rInt <* char ',' <*> rInt)

rColorMove :: ReadP Move
rColorMove = COLOR <$> rBlockId <*> rColor

rColor :: ReadP Color
rColor = readS_to_P reads

rSwapMove :: ReadP Move
rSwapMove = SWAP <$> rBlockId <*> rBlockId

rMergeMove :: ReadP Move
rMergeMove = MERGE <$> rBlockId <*> rBlockId

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

-- World

data World 
    = World
    { prog        :: [Instruction]
    , counter     :: Int
    , blocks      :: Map.Map BlockId Block
    , pict        :: Gloss.Picture
    }

instance Show World where
    showsPrec _ = \ case
        wolrd@(World { blocks = btbl }) -> shows btbl

initialWorld :: [Instruction] -> World
initialWorld is
    = World
    { prog = is
    , counter = 0
    , blocks = Map.singleton [0] 
                 (SimpleBlock (Rectangle (0,0) (400, 400)) white)
    , pict   = undefined
    }

white :: Color
white = (255,255,255,255)

incCount :: World -> (Int, World)
incCount world = (cnt, world { counter = succ cnt })
    where
        cnt = counter world

type Instruction = World -> World
