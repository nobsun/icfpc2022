module Sandbox where

import Data.Word

-- | static model

data BlockID = BlockID [Int] deriving (Show, Eq)
-- 座標
type X = Int
type Y = Int
-- 位置(絶対座標)
data Pos = Pos { posX :: X
               , posY :: Y
               } deriving (Show, Eq)

data Color
  = Color { r :: Int
          , g :: Int
          , b :: Int
          , a :: Int
          } deriving (Show, Eq)

-- | NOTE: Shape は不明
type Width  = Int
type Height = Int
data Shape = Rect { w :: Width
                  , h :: Height
                  } deriving (Show, Eq)

data Simple = Simple { sPos     :: Pos
                     , sBlockId :: BlockID
                     , sShape   :: Shape
                     , sColor   :: Color
                     } deriving (Show, Eq)

data Complex a = Complex { cPos    :: Pos
                         , cShape  :: Shape
                         , cChilds :: [a]
                         } deriving (Show, Eq)

data Block = Leaf Simple           -- ^ SimpleBlock
           | Node (Complex Block)  -- ^ ComplexBlock
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
-- >>> let b = Leaf (Simple (Pos 0 0) (BlockID [0]) (Rect 400 400) (Color 0 0 0 0))
-- >>> cataBlock Leaf Node b == b
-- True
--
-- >>> let l = Leaf (Simple (Pos   0 0) (BlockID [0, 0]) (Rect 200 400) (Color 0 0 0 0))
-- >>> let r = Leaf (Simple (Pos 200 0) (BlockID [0, 1]) (Rect 200 400) (Color 0 0 0 0))
-- >>> let b = Node (Complex (Pos 0 0) (Rect 400 400) [l, r])
-- >>> cataBlock Leaf Node b == b
-- True
cataBlock :: (Simple -> a) -> (Complex a -> a) -> Block -> a
cataBlock f g = u where
  u (Leaf a)                = f a
  u (Node (Complex p s bs)) = g (Complex p s (map u bs))

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
-- >>> let b = Leaf (Simple (Pos 0 0) (BlockID [0]) (Rect 400 400) (Color 0 0 0 0))
-- >>> anaBlock psi b == b
-- True
--
-- >>> let l = Leaf (Simple (Pos   0 0) (BlockID [0, 0]) (Rect 200 400) (Color 0 0 0 0))
-- >>> let r = Leaf (Simple (Pos 200 0) (BlockID [0, 1]) (Rect 200 400) (Color 0 0 0 0))
-- >>> let b = Node (Complex (Pos 0 0) (Rect 400 400) [l, r])
-- >>> anaBlock psi b == b
-- True
anaBlock :: (a -> Either Simple (Complex a)) -> a -> Block
anaBlock psi = v where
  v = either Leaf (Node . f) . psi
  f (Complex p s xs) = Complex p s (map v xs)

-- | moves

-- Vertical   : 垂直線を引いてカットつまり横に分割
-- Horizontal : 水平線を引いてカットつまり縦に分割
data Orientation = Vertical | Horizontal deriving (Show, Eq)
-- 幅とか高さ
type Size = Int

-- Line
type Line = (Orientation, Size)

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
