
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import qualified Graphics.Gloss as Gloss
import Numeric
import Text.ParserCombinators.ReadP


type Canvas = [Block]

type Color = (Int, Int, Int, Int)

type Block        = Either SimpleBlock ComplexBlock
data SimpleBlock  = SimpleBlock Shape Color
data ComplexBlock = ComplexBlock Shape ChildBlocks
type ChildBlocks  = [SimpleBlock]

data Shape -- Not yet defined

type Id = Int
type BlockId = [Int]

instance {-# Overlapping #-} Read BlockId where
    readsPrec _ = readP_to_S rBlockId

rBlockId :: ReadP BlockId
rBlockId = undefined

data Orientation
    = X
    | Y
    deriving (Eq, Show, Read)

rOrientation :: ReadP Orientation
rOrientation = readS_to_P reads

type Offset = Int

data Move 
    = LCUT  BlockId Orientation Offset
    | PCUT  BlockId Offset Offset
    | COLOR BlockId Types.Color
    | SWAP  BlockId BlockId 
    | MERGE BlockId BlockId 
    deriving (Eq, Show)

instance Read Move where
    readsPrec _ = readP_to_S rMove

rMove :: ReadP Move
rMove = rLCut +++ rPCut +++ rColor +++ rSwap +++ rMerge

rLCut :: ReadP Move
rLCut = LCUT <$> rBlockId <*> rOrientation <*> rOffset

rOffset :: ReadP Offset
rOffset = readS_to_P readDec

rPCut :: ReadP Move
rPCut = PCUT <$> rBlockId <*> rOffset <*> rOffset

rColor :: ReadP Move
rColor = COLOR <$> rBlockId <*> rColor'

rColor' :: ReadP Color
rColor' = readS_to_P reads

gColor :: Color -> Gloss.Color
gColor (r,g,b,a) = Gloss.makeColorI r g b a

rSwap :: ReadP Move
rSwap = SWAP <$> rBlockId <*> rBlockId

rMerge :: ReadP Move
rMerge = MERGE <$> rBlockId <*> rBlockId

data ProgLine
    = Move Move
    | Newline
    | Comment String
    deriving (Eq, Show)

data World 
    = World
    { blockSupply :: BlockId
    , canvas      :: Canvas
    , picture     :: Gloss.Picture
    }
