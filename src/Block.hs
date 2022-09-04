{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Block where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Bitmap as Gloss
import Types

type BitmapData = Gloss.BitmapData
type Canvas = Shape

data Block
    = SimpleBlock { shape :: Shape, blockColor :: Color }
    | ComplexBlock { shape :: Shape, children :: ChildBlocks }
    deriving (Eq, Show)

shapingBlock :: Shape -> Block -> Maybe Block
shapingBlock shp b = case b of
    SimpleBlock shp1 col -> 
        maybe Nothing (Just . flip SimpleBlock col) (intersectShape shp shp1)
    ComplexBlock shp1 bs -> 
        intersectShape shp shp1 >>= \ shp2 ->
            case mapMaybe (shapingBlock shp) bs of
                []   -> error "imposible!"
                b:[] -> Just b
                bs   -> Just $ ComplexBlock shp2 bs

type ChildBlocks  = [Block]

type BlockTable = Map.Map BlockId Block

dispBlockEntry :: Map.Map BlockId Block -> (BlockId, Block) -> String
dispBlockEntry tbl (bid, b) = dispBlockId bid ++ ": " ++ dispBlock b

dispBlock :: Block -> String
dispBlock = \ case
    SimpleBlock  shp col -> show shp ++ " " ++ dispColor col ++ " size: " ++ show (shapeSize shp)
    ComplexBlock shp bs  -> show shp ++ " [" ++ intercalate ", " (map dispBlock bs) ++ "]"

data World
    = World
    { canvas      :: Shape
    , prog        :: [Instruction]
    , counter     :: Int
    , blocks      :: BlockTable
    , pict        :: Gloss.Picture
    , costs       :: !Int
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
    , counter = 1
    , blocks = Map.singleton (V.singleton 0)
                 (SimpleBlock (Rectangle (0,0) (400, 400)) white)
    , pict   = undefined
    , costs  = 0
    }

initialWorld :: World
initialWorld = initializeWorld (Rectangle (0,0) (400,400)) []

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

