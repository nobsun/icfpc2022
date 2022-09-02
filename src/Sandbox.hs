module Sandbox where

import Data.Word

-- | static model

type BlockID = [Int]

-- | 簡易版
data Color = White | Black | Red deriving (Show, Eq)
{-
data Color
  = RGBA { r :: Word8
         , g :: Word8
         , b :: Word8
         , a :: Word8
         } deriving (Show, Eq)
-}

-- | NOTE: Shape は不明
type Width  = Int
type Height = Int
type Shape = (Width, Height)

data Block = Leaf (BlockID, Shape, Color)       -- ^ SimpleBlock
           | Node (Shape, [Block])              -- ^ ComplexBlock
           deriving (Show, Eq)

-- | NOTE: Node ケースで rhs の式における s の置き場所は Shape の意味によっては変える可能性あり
--
--                [leaf, node]
--          T  <---------------- A + [T]
--          |                      |
--          |                      |
--  (|f, g|)|                      | 1_A + map (|f, g|)
--          |                      |
--          v                      v
--          X  <---------------- A + [X]
--                  [f, g]
--        
-- >>> let b = Leaf ([0], (400, 400), White)
-- >>> cataBlock Leaf Node b == b
-- True
--
-- >>> let l = Leaf ([0, 0], (200, 400), White)
-- >>> let r = Leaf ([0, 1], (200, 400), White)
-- >>> let b = Node ((400, 400), [l, r])
-- >>> cataBlock Leaf Node b == b
-- True
cataBlock :: ((BlockID, Shape, Color) -> a) -> ((Shape, [a]) -> a) -> Block -> a
cataBlock f g = u
  where
    u (Leaf a)       = f a
    u (Node (s, bs)) = g (s, map u bs)

-- | NOTE: Node ケースで rhs の式における s の置き場所は Shape の意味によっては変える可能性あり
--
--                [leaf, node]
--          T  <---------------- A + [T]
--          ^                      ^
--          |                      |
--  [(psi)] |                      | 1_A + map [(psi)]
--          |                      |
--          |                      |
--          X  ----------------> A + [X]
--                  psi
--
-- >>> :{
-- >>> psi (Leaf a) = Left a
-- >>> psi (Node b) = Right b
-- >>> :}
--
-- >>> let b = Leaf ([0], (400, 400), White)
-- >>> anaBlock psi b == b
-- True
--
-- >>> let l = Leaf ([0, 0], (200, 400), White)
-- >>> let r = Leaf ([0, 1], (200, 400), White)
-- >>> let b = Node ((400, 400), [l, r])
-- >>> anaBlock psi b == b
-- True
anaBlock :: (a -> Either (BlockID, Shape, Color) (Shape, [a])) -> a -> Block
anaBlock psi = v
  where
    v x = case psi x of
      Left  b       -> Leaf b
      Right (s, xs) -> Node (s, map v xs)

-- | moves

-- Vertical   : 垂直線を引いてカットつまり横に分割
-- Horizontal : 水平線を引いてカットつまり縦に分割
data Orientation = Vertical | Horizontal deriving (Show, Eq)
-- 幅とか高さ
type Size = Int

-- Line
type Line = (Orientation, Size)

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
