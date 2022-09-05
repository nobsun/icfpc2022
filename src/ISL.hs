{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module ISL where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import qualified Data.Vector.Generic as V
import Text.ParserCombinators.ReadP

import Types
import Block
import World

import Debug.Trace

fetch :: World -> (Instruction, World)
fetch world = ( head (prog world)
              , world { prog = tail (prog world)}
              )

interp :: Move -> Instruction
interp mv world = case mv of
    LCUT bid o off -> lcut bid o off world
    PCUT bid point -> pcut bid point world
    COLOR bid col  -> colormove bid col world
    SWAP bid1 bid2 -> swapmove bid1 bid2 world
    MERGE bid1 bid2 -> mergemove bid1 bid2 world

{- | lcut : Line Cut Move instruction 
>>> lcut (V.fromList [0] :: BlockId) X 100 sampleWorld
[0.0]: Rectangle {leftBottom = (0,0), rightUpper = (100,400)} [255,255,255,255] size: 40000
[0.1]: Rectangle {leftBottom = (100,0), rightUpper = (400,400)} [255,255,255,255] size: 120000
<BLANKLINE>
>>> lcut (V.fromList [0] :: BlockId) Y 100 sampleWorld
[0.0]: Rectangle {leftBottom = (0,0), rightUpper = (400,100)} [255,255,255,255] size: 40000
[0.1]: Rectangle {leftBottom = (0,100), rightUpper = (400,400)} [255,255,255,255] size: 120000
<BLANKLINE>
-}
lcut :: BlockId -> Orientation -> Offset -> Instruction
lcut bid o off world = case world of
    World { canvas = cnvs, blocks  = tbl0, costs = tc } -> world { blocks = tbl3, costs = tc + c }
        where
            b0@(bid0, block0) = (V.snoc bid 0, sub0)
            b1@(bid1, block1) = (V.snoc bid 1, sub1)
            (sub0, sub1) = case block of
                SimpleBlock _ col -> (SimpleBlock shp0 col, SimpleBlock shp1 col)
                _ -> ( fromMaybe (error "lcut: 0?") (shapingBlock shp0 block)
                     , fromMaybe (error "lcut: 1?") (shapingBlock shp1 block)
                     )
                where
                    msg = show (shape block) ++ " " ++ show shp1
            block = tbl0 Map.! bid
            (x0,y0) = leftBottom (shape block)
            (x1,y1) = rightUpper (shape block)
            (x00,y00,x01,y01,x10,y10,x11,y11) = case o of
                X -> (x0,y0,off,y1,off,y0,x1,y1)
                Y -> (x0,y0,x1,off,x0,off,x1,y1)
            shp0 = Rectangle { leftBottom = (x00,y00)
                             , rightUpper = (x01,y01)
                             }
            shp1 = Rectangle { leftBottom = (x10,y10)
                             , rightUpper = (x11,y11)
                             }
            tbl1 = uncurry Map.insert b0 tbl0
            tbl2 = uncurry Map.insert b1 tbl1
            tbl3 = Map.delete bid tbl2
            c = cost (size cnvs) 7 (shape block)

{- | pcut : Point Cut Move instruction
>>> import Control.Arrow
>>> bs  = map (first V.fromList) [([1], SimpleBlock (Rectangle (0,0) (40, 50)) green), ([2], SimpleBlock (Rectangle (0,50) (40, 400)) red), ([3], SimpleBlock (Rectangle (40,0) (400, 400)) white)]
>>> tbl = foldr (uncurry Map.insert) Map.empty bs
>>> world0 = sampleWorld
>>> world1 = world0 { blocks = tbl }
>>> world1
[1]: Rectangle {leftBottom = (0,0), rightUpper = (40,50)} [0,255,0,255] size: 2000
[2]: Rectangle {leftBottom = (0,50), rightUpper = (40,400)} [255,0,0,255] size: 14000
[3]: Rectangle {leftBottom = (40,0), rightUpper = (400,400)} [255,255,255,255] size: 144000
<BLANKLINE>
>>> pcut (V.fromList [1] :: BlockId) (30, 30) world1
[1.0]: Rectangle {leftBottom = (0,0), rightUpper = (30,30)} [0,255,0,255] size: 900
[1.1]: Rectangle {leftBottom = (30,0), rightUpper = (40,30)} [0,255,0,255] size: 300
[1.2]: Rectangle {leftBottom = (30,30), rightUpper = (40,50)} [0,255,0,255] size: 200
[1.3]: Rectangle {leftBottom = (0,30), rightUpper = (30,50)} [0,255,0,255] size: 600
[2]: Rectangle {leftBottom = (0,50), rightUpper = (40,400)} [255,0,0,255] size: 14000
[3]: Rectangle {leftBottom = (40,0), rightUpper = (400,400)} [255,255,255,255] size: 144000
<BLANKLINE>
-}
pcut :: BlockId -> Point -> Instruction
pcut bid (mx,my) world = case world of
    World { canvas = cnvs, blocks = tbl0, costs = tc } -> world { blocks = tbl2, costs = tc + c }
        where
            b0@(bid0, block0) = (V.snoc bid 0, sub0)
            b1@(bid1, block1) = (V.snoc bid 1, sub1)
            b2@(bid2, block2) = (V.snoc bid 2, sub2)
            b3@(bid3, block3) = (V.snoc bid 3, sub3)
            (sub0,sub1,sub2,sub3) = case block of
                SimpleBlock _ col -> (SimpleBlock shp0 col, SimpleBlock shp1 col, SimpleBlock shp2 col, SimpleBlock shp3 col)
                _ -> ( fromMaybe (error "pcut: 0?") (shapingBlock shp0 block)
                     , fromMaybe (error "pcut: 1?") (shapingBlock shp1 block)
                     , fromMaybe (error "pcut: 2?") (shapingBlock shp2 block)
                     , fromMaybe (error "pcut: 3?") (shapingBlock shp3 block)
                     )
            block = tbl0 Map.! bid
            (x0,y0) = leftBottom (shape block)
            (x1,y1) = rightUpper (shape block)
            shp0 = Rectangle (x0,y0) (mx,my)
            shp1 = Rectangle (mx,y0) (x1,my)
            shp2 = Rectangle (mx,my) (x1,y1)
            shp3 = Rectangle (x0,my) (mx,y1)
            tbl1 = foldr (uncurry Map.insert) tbl0 [b0, b1, b2, b3]
            tbl2 = Map.delete bid tbl1 
            c = cost (size cnvs) 10 (shape block)

colormove :: BlockId -> Color -> Instruction
colormove bid col world = case world of
    World { canvas = cnvs, blocks = tbl0, costs = tc } 
        -> world { blocks = setColor col bid (blocks world), costs = {- trace msg -} tc + c}
            where
                c = cost (size cnvs) 5 shp
                shp = shape (tbl0 Map.! bid)
                msg = "-------> " ++ dispBlockId bid ++ " " ++ show shp ++ " size: " ++ show (size shp)

setColor :: Color -> BlockId -> Map.Map BlockId Block -> Map.Map BlockId Block
setColor c bid tbl = Map.update (const $ Just $ setColor' c (tbl Map.! bid)) bid tbl

setColor' :: Color -> Block -> Block
setColor' c = \ case
    SimpleBlock shp _   -> SimpleBlock shp c
    ComplexBlock shp bs -> ComplexBlock shp (map (setColor' c) bs)

swapmove :: BlockId -> BlockId -> Instruction
swapmove bid0 bid1 world = case world of
    World { canvas = cnvs, blocks = tbl0, costs = tc }
        | sameShape shp0 shp1 -> world { blocks = tbl2, costs = tc + c }
        | otherwise           -> error "swapmove: not same shapes"
        where
            block0 = tbl0 Map.! bid0
            block1 = tbl0 Map.! bid1
            shp0 = shape block0
            shp1 = shape block1
            tbl1 = Map.update (const $ Just $ block0 { shape = shp1 }) bid0 tbl0
            tbl2 = Map.update (const $ Just $ block1 { shape = shp0 }) bid1 tbl1
            c = cost (size cnvs) 3 shp0

mergemove :: BlockId -> BlockId -> Instruction
mergemove bid0 bid1 world = case world of
    World { canvas = cnvs, counter = cnt, blocks = tbl0, costs = tc }
        | compatibleShape shp0 shp1 -> world { counter = succ cnt, blocks = tbl2, costs = tc + c}
        | otherwise                 -> error "mergemove: not compatible shapes"
        where
            block0 = tbl0 Map.! bid0
            block1 = tbl0 Map.! bid1
            shp0@(Rectangle (x00,y00) (x01,y01)) = shape block0
            shp1@(Rectangle (x10,y10) (x11,y11)) = shape block1
            newshp = if
                | x00 == x10 -> Rectangle (x00, min y00 y10) (x01, max y01 y11)
                | y00 == y10 -> Rectangle (min x00 x10, y00) (max x01 x11, y01)
                | otherwise  -> error "mergemove: not compatible shapes"
            tbl1 = Map.insert (V.singleton cnt) (ComplexBlock newshp [block0, block1]) tbl0
            tbl2 = foldr Map.delete tbl1 [bid0, bid1]
            c0 = cost (size cnvs) 1 shp0
            c1 = cost (size cnvs) 1 shp1
            c  = max c0 c1

cost :: Int -> Int -> Shape -> Int
cost canvasSize baseCost shape
    = roundUpOn5 $ fromIntegral baseCost * fromIntegral canvasSize / fromIntegral (size shape)

size :: Shape -> Int
size = \ case
    Rectangle (x0, y0) (x1, y1) -> (x1 - x0) * (y1 - y0)

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

sample :: String
sample
    = unlines
    [ "cut [0] [x] [200]"
    , "color [0.1] [255, 0, 0, 255]"
    , "merge [0.0] [0.1]"
    , "cut [1] [y] [200]"
    , "cut [1.0] [y] [100]"
    , "color [1.0.1] [0,255,0,255]"
    ]

{- | local load
>>> localLoad sample
[cut [0] [X] [200],color [0.1] [255,0,0,255],merge [0.0] [0.1],cut [1] [Y] [200],cut [1.0] [Y] [100],color [1.0.1] [0,255,0,255]]
-}
localLoad :: String -> [Move]
localLoad src = [ read (filter (' '/=) l) | l <- lines src, not (null l || "#" `isPrefixOf` l) ] 

localEvalTrace :: World -> [World]
localEvalTrace world = world : rests
    where
        rests
            | isFinal world = []
            | otherwise     = localEvalTrace (next world)

next :: World -> World
next = uncurry ($) . fetch

isFinal :: World -> Bool
isFinal = \ case
    World { prog = is } -> null is


