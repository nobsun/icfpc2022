module Sandbox where

import Data.Word

-- | static model

type BlockID = [Int]

data Color
  = RGBA { r :: Word8
         , g :: Word8
         , b :: Word8
         , a :: Word8
         } deriving (Show, Eq)

type Shape = Int

data SimpleBlock = SimpleBlock BlockID Shape Color
  deriving (Show, Eq)

data ComplexBlock = ComplexBlock Shape ChildBlocks
  deriving (Show, Eq)

type ChildBlocks = [Block]

type Block = Either SimpleBlock ComplexBlock

-- | moves

-- Vertical   : 垂直線を引いてカットつまり横に分割
-- Horizontal : 水平線を引いてカットつまり縦に分割
data Direction = Vertical | Horizontal deriving (Show, Eq)
-- 幅とか高さ
type Size = Int

-- Line
type Line = (Direction, Size)

-- 座標
type X = Int
type Y = Int
-- Point
type Point = (Int, Int) -- X, Y

line :: BlockID      -- ^ Line Cut Move するブロック
     -> Line         -- ^ Line
     -> Block        -- ^ 元の全体
     -> Block        -- ^ 新しく出来た全体
line = undefined

point :: BlockID     -- ^ Point Cut Move するブロック
      -> Point       -- ^ Point
      -> Block       -- ^ 元の全体
      -> Block       -- ^ 新しく出来た全体
point = undefined


color :: BlockID     -- ^ Color Move するブロック
      -> Color       -- ^ Color
      -> Block       -- ^ 元の全体
      -> Block       -- ^ 新しく出来た全体
color = undefined
      
swap :: BlockID      -- ^ Swap Move する一方
     -> BlockID      -- ^ Swap Move するもう一方
     -> Block        -- ^ 元の全体
     -> Block        -- ^ 新しく出来た全体
swap = undefined

merge :: BlockID     -- ^ Merge Move する一方
      -> BlockID     -- ^ Merge Move するもう一方
      -> Block       -- ^ 元の全体
      -> Block       -- ^ 新しく出来た全体
merge = undefined
