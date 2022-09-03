{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module ISL where

import qualified Data.Map as Map
import Data.Maybe
import Types
import Text.ParserCombinators.ReadP

rProgram :: ReadP [ProgLine]
rProgram = sepBy1 rProgramLine rNewline <* eof 

rNewline :: ReadP ProgLine
rNewline = Newline <$ char '\n'

rProgramLine :: ReadP ProgLine
rProgramLine = rNewline +++ rComment +++ (Move <$> rMove)

rComment :: ReadP ProgLine
rComment = Comment <$> (char '#' *> munch (/= '\n'))

--

fetch :: World -> (Instruction, World)
fetch world = ( head (prog world)
              , world { prog = tail (prog world)}
              )

interp :: ProgLine -> Instruction
interp pl world = case pl of
    Newline     -> world
    Comment msg -> world
    Move mv -> case mv of
        LCUT bid o off -> lcut bid o off world
        PCUT bid point -> pcut bid point world
        COLOR bid col  -> colormove bid col world
        SWAP bid1 bid2 -> swapmove bid1 bid2 world
        MERGE bid1 bid2 -> mergemove bid1 bid2 world

{- | lcut : Line Cut Move instruction 
>>> lcut [0] X 100 initialWorld
[0.0]: Rectangle {leftBottom = (0,0), rightUpper = (100,400)} [255,255,255,255]
[0.1]: Rectangle {leftBottom = (100,0), rightUpper = (400,400)} [255,255,255,255]
<BLANKLINE>
>>> lcut [0] Y 100 initialWorld
[0.0]: Rectangle {leftBottom = (0,0), rightUpper = (400,100)} [255,255,255,255]
[0.1]: Rectangle {leftBottom = (0,100), rightUpper = (400,400)} [255,255,255,255]
<BLANKLINE>
-}
lcut :: BlockId -> Orientation -> Offset -> Instruction
lcut bid o off world = case world of
    World { canvas = cnvs, blocks  = tbl0, costs = cs } -> world { blocks = tbl3, costs = c:cs }
        where
            b0@(bid0, block0) = (0:bid, SimpleBlock shp0 col)
            b1@(bid1, block1) = (1:bid, SimpleBlock shp1 col)
            block = tbl0 Map.! bid
            col = blockColor block
            (x0,y0) = leftBottom (shape block)
            (x1,y1) = rightUpper (shape block)
            (x00,y00,x01,y01,x10,y10,x11,y11) = case o of
                X -> (x0,y0,x0+off,y1,x0+off,y0,x1,y1)
                Y -> (x0,y0,x1,y0+off,x0,y0+off,x1,y1)
            shp0 = Rectangle { leftBottom = (x00,y00)
                             , rightUpper = (x01,y01)
                             }
            shp1 = Rectangle { leftBottom = (x10,y10)
                             , rightUpper = (x11,y11)
                             }
            tbl1 = uncurry Map.insert b0 tbl0
            tbl2 = uncurry Map.insert b1 tbl1
            tbl3 = Map.delete bid tbl2
            c = cost (size cnvs) 7 tbl0 bid

{- | pcut : Point Cut Move instruction
>>> bs  = [([1], SimpleBlock (Rectangle (0,0) (40, 50)) green), ([2], SimpleBlock (Rectangle (0,50) (40, 400)) red), ([3], SimpleBlock (Rectangle (40,0) (400, 400)) white)]
>>> tbl = foldr (uncurry Map.insert) Map.empty bs
>>> world0 = initialWorld
>>> world1 = world0 { blocks = tbl }
>>> world1
[1]: Rectangle {leftBottom = (0,0), rightUpper = (40,50)} [0,255,0,255]
[2]: Rectangle {leftBottom = (0,50), rightUpper = (40,400)} [255,0,0,255]
[3]: Rectangle {leftBottom = (40,0), rightUpper = (400,400)} [255,255,255,255]
<BLANKLINE>
>>> pcut [1] (30, 30) world1
[1.0]: Rectangle {leftBottom = (0,0), rightUpper = (30,30)} [0,255,0,255]
[1.1]: Rectangle {leftBottom = (30,0), rightUpper = (40,30)} [0,255,0,255]
[2]: Rectangle {leftBottom = (0,50), rightUpper = (40,400)} [255,0,0,255]
[1.2]: Rectangle {leftBottom = (30,30), rightUpper = (40,50)} [0,255,0,255]
[3]: Rectangle {leftBottom = (40,0), rightUpper = (400,400)} [255,255,255,255]
[1.3]: Rectangle {leftBottom = (0,30), rightUpper = (30,50)} [0,255,0,255]
<BLANKLINE>
-}
pcut :: BlockId -> Point -> Instruction
pcut bid (mx,my) world = case world of
    World { canvas = cnvs, blocks = tbl0, costs = cs } -> world { blocks = tbl2, costs = c:cs }
        where
            b0@(bid0, block0) = (0:bid, SimpleBlock shp0 col)
            b1@(bid1, block1) = (1:bid, SimpleBlock shp1 col)
            b2@(bid2, block2) = (2:bid, SimpleBlock shp2 col)
            b3@(bid3, block3) = (3:bid, SimpleBlock shp3 col)
            block = tbl0 Map.! bid
            col = blockColor block 
            (x0,y0) = leftBottom (shape block)
            (x1,y1) = rightUpper (shape block)
            shp0 = Rectangle (x0,y0) (mx,my)
            shp1 = Rectangle (mx,y0) (x1,my)
            shp2 = Rectangle (mx,my) (x1,y1)
            shp3 = Rectangle (x0,my) (mx,y1)
            tbl1 = foldr (uncurry Map.insert) tbl0 [b0, b1, b2, b3]
            tbl2 = Map.delete bid tbl1 
            c = cost (size cnvs) 10 tbl0 bid

colormove :: BlockId -> Color -> Instruction
colormove bid col world = case world of
    World { canvas = cvs, blocks = tbl0, costs = cs } 
        -> world { blocks = setColor col bid (blocks world) , costs = c:cs}
            where
                c = cost (size cvs) 5 tbl0 bid 

setColor :: Color -> BlockId -> Map.Map BlockId Block -> Map.Map BlockId Block
setColor c bid tbl = case tbl Map.! bid of
    SimpleBlock shp _   -> Map.update (const $ Just $ SimpleBlock shp c) bid tbl
    ComplexBlock shp bs -> foldr (setColor c) tbl bs

swapmove :: BlockId -> BlockId -> Instruction
swapmove bid1 bid2 world = case world of
    World {} -> undefined

mergemove :: BlockId -> BlockId -> Instruction
mergemove bid1 bid2 world = case world of
    World {} -> undefined



cost :: Int -> Int -> BlockTable -> BlockId -> Int
cost csz base tbl bid 
    = roundUpOn5 $ fromIntegral base * fromIntegral csz / fromIntegral (size (shape block))
    where
        block = tbl Map.! bid

size :: Shape -> Int
size = \ case
    Rectangle (x0, y0) (x1, y1) -> succ (x1 - x0) * succ (y1 - y0)

{- | roundUpOn5: 四捨五入
>>> roundUpOn5 4.5
5
>>> roundUpOn5 5.5
6
-}
roundUpOn5 :: (RealFrac a, Integral b) => a -> b
roundUpOn5 x = if
    | n <= -0.5 -> m - 1
    | n >=  0.5 -> m + 1
    | otherwise -> m
    where (m, n) = properFraction x
