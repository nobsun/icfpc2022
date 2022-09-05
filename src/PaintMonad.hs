{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module PaintMonad
  ( genMoves

  , getBlockShape

  , emitMove
  , lcut
  , pcut
  , lcutRel
  , pcutRel
  , lcutN
  , color
  , swap
  , merge
  , mergeN
  , pmerge
  , focus
  , fillRect

  , sample_problem_1
  , sample_problem_2
  , sample_problem_3
  ) where

import Codec.Picture
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector.Generic as V

import Types

newtype E a = E (Either String a) deriving (Functor, Applicative, Monad)

instance MonadFail E where
  fail s = E (Left s)

type Paint = RWST Shape (Seq.Seq Move) (Int, Map BlockId Shape) E

genMoves :: InitialConfig -> ([BlockId] -> Paint a) -> ([Move], Int)
genMoves config f =
  case runRWST (f [icbBlockIdParsed block | block <- icBlocks config]) r0 s0 of
    E (Left err) -> error err
    E (Right (_, (cnt', _), moves)) -> (F.toList moves, cnt')
  where
    r0 = Rectangle (0,0) (icWidth config, icHeight config)
    s0 =
      ( icCounter config
      , Map.fromList [(icbBlockIdParsed block, icbShape block) | block <- icBlocks config]
      )

getBlockShape :: BlockId -> Paint Shape
getBlockShape bid = do
  (_, m) <- get
  case Map.lookup bid m of
    Nothing -> fail ("no such block: " ++ show bid)
    Just s -> return s

modifyBlocks :: (Map BlockId Shape -> Map BlockId Shape) -> Paint ()
modifyBlocks f = modify g
  where
    g (cnt, m) = seq m' (cnt, m')
      where
        m' = f m

debugCheckBlockShape :: BlockId -> Shape -> String -> Paint ()
debugCheckBlockShape bid shape msg = do
  shape1 <- getBlockShape bid
  unless (shape == shape1) $ fail ("shape mismatch: " ++ msg)

emitMove :: Move -> Paint ()
emitMove move = tell (Seq.singleton move)

lcut :: BlockId -> Orientation -> Int -> Paint (BlockId, BlockId)
lcut bid dir offset = do
  let move = LCUT bid dir offset
  Rectangle (x1,y1) (x2,y2) <- getBlockShape bid
  case dir of
    X -> do
      unless (x1 <= offset && offset <= x2) $
        fail ("invalid move (" ++ dispMove move ++ "): " ++ show offset ++ " not in " ++ show x1 ++ ".." ++ show x2)
      modifyBlocks $
        Map.insert (V.snoc bid 0) (Rectangle (x1,y1) (offset,y2)) .
        Map.insert (V.snoc bid 1) (Rectangle (offset,y1) (x2,y2)) .
        Map.delete bid
    Y -> do
      unless (y1 <= offset && offset <= y2) $
        fail ("invalid move (" ++ dispMove move ++ "): " ++ show offset ++ " not in " ++ show y1 ++ ".." ++ show y2)
      modifyBlocks $
        Map.insert (V.snoc bid 0) (Rectangle (x1,y1) (x2,offset)) .
        Map.insert (V.snoc bid 1) (Rectangle (x1,offset) (x2,y2)) .
        Map.delete bid
  emitMove move
  return (V.snoc bid 0, V.snoc bid 1)

pcut :: BlockId -> Point -> Paint (BlockId, BlockId, BlockId, BlockId)
pcut bid point@(x1,y1) = do
  let move = PCUT bid point
  shape@(Rectangle (x0,y0) (x2,y2)) <- getBlockShape bid
  unless (x0 < x1 && x1 < x2 && y0 < y1 && y1 < y2) $
    fail ("invalid move (" ++ dispMove move ++ "): " ++ show (x1,y1) ++ " not in " ++ show shape)
  modifyBlocks $
    Map.insert (V.snoc bid 0) (Rectangle (x0, y0) (x1, y1)) .
    Map.insert (V.snoc bid 1) (Rectangle (x1, y0) (x2, y1)) .
    Map.insert (V.snoc bid 2) (Rectangle (x1, y1) (x2, y2)) .
    Map.insert (V.snoc bid 3) (Rectangle (x0, y1) (x1, y2)) .
    Map.delete bid
  emitMove move
  return (V.snoc bid 0, V.snoc bid 1, V.snoc bid 2, V.snoc bid 3)

lcutRel :: BlockId -> Orientation -> Int -> Paint (BlockId, BlockId)
lcutRel bid dir offset = do
  Rectangle (x0,y0) _ <- getBlockShape bid
  lcut bid dir (if dir == X then x0 + offset else y0 + offset)

pcutRel :: BlockId -> Point -> Paint (BlockId, BlockId, BlockId, BlockId)
pcutRel bid (x,y) = do
  Rectangle (x0,y0) _ <- getBlockShape bid
  pcut bid (x0+x, y0+y)

lcutN :: BlockId -> Orientation -> [Int] -> Paint [BlockId]
lcutN bid dir [] = return [bid]
lcutN bid dir (x : xs) = do
  (b1, b2) <- lcut bid dir x
  bs <- lcutN b2 dir xs
  return (b1 : bs)

color :: BlockId -> Color -> Paint ()
color bid color = do
  _ <- getBlockShape bid
  emitMove $ COLOR bid color

swap :: BlockId -> BlockId -> Paint ()
swap bid1 bid2 = do
  s1 <- getBlockShape bid1
  s2 <- getBlockShape bid1
  modifyBlocks $ Map.insert bid1 s2 . Map.insert bid2 s1
  emitMove $ SWAP bid1 bid2

merge :: BlockId -> BlockId -> Paint BlockId
merge bid1 bid2 = do
  let move = MERGE bid1 bid2
  s1 <- getBlockShape bid1
  s2 <- getBlockShape bid2
  case mergeShape s1 s2 of
    Nothing -> fail ("invalid move (" ++ dispMove move ++ "): (" ++ show s1 ++ "), (" ++ show s2 ++ ")")
    Just s3 -> do
      (n, blocks) <- get
      put (n+1, blocks)
      let bid3 = V.singleton (n+1)
      modifyBlocks $
        Map.delete bid1 .
        Map.delete bid2 .
        Map.insert bid3 s3
      emitMove move
      return (V.singleton (n+1))

mergeN :: [BlockId] -> Paint BlockId
mergeN [] = undefined
mergeN (b : bs) = f b bs
 where
   f b1 [] = return b1
   f b1 (b2 : bs) = do
     b <- merge b1 b2
     f b bs

pmerge :: (BlockId, BlockId, BlockId, BlockId) -> Paint BlockId
pmerge (b1, b2, b3, b4) = do
  bot <- merge b1 b2
  top <- merge b3 b4
  merge bot top

focus :: BlockId -> Shape -> (BlockId -> Paint BlockId) -> Paint BlockId
focus parent s1@(Rectangle bl1@(x1,y1) tr1@(x1',y1')) f = do
  s0@(Rectangle bl0@(x0,y0) tr0@(x0',y0')) <- getBlockShape parent

  if | s1 == s0 -> f parent

     -- 下に張り付いている場合
     | y1 == y0 -> do
         if x1' == x0' then do
           -- 右下に張り付いている場合
           (b1,b2,b3,b4) <- pcut parent (x1,y1') -- 左上の点で分割
           debugCheckBlockShape b2 s1 "A"
           b2 <- f b2
           pmerge (b1,b2,b3,b4)
         else do
           (b1,b2,b3,b4) <- pcut parent tr1 -- 右上の点で分割
           b1 <-
             if x1 == x0 then do
               -- 左下に張り付いている場合
               debugCheckBlockShape b1 s1 "B"
               f b1
             else do
               -- 右下にも左下にも張り付いていない場合
               (b1l,b1r) <- lcut b1 X x1
               debugCheckBlockShape b1r s1 "C"
               b1r <- f b1r
               merge b1l b1r
           pmerge (b1,b2,b3,b4)

     -- 上に張り付いている場合
     | y1' == y0' -> do
         if x1 == x0 then do
           -- 左上に張り付いている場合
           (b1,b2,b3,b4) <- pcut parent (x1',y1) -- 右下の点で分割
           debugCheckBlockShape b4 s1 "D"
           b4 <- f b4
           pmerge (b1,b2,b3,b4)
         else do
           (b1,b2,b3,b4) <- pcut parent bl1
           b3 <-
             if x1' == x0' then do
               -- 右上に張り付いている場合
               debugCheckBlockShape b3 s1 "E"
               f b3
             else do
               -- 左上にも右上にも張り付いていない場合
               (b3l,b3r) <- lcut b3 X x1
               debugCheckBlockShape b3r s1 "F"
               b3r <- f b3r
               merge b3l b3r
           pmerge (b1,b2,b3,b4)

     -- 左にだけ張り付いている場合
     | x1 == x0 -> do
         (b1,b2,b3,b4) <- pcut parent tr1 -- 右上の点で分割
         (b1b, b1t) <- lcut b1 Y y1
         debugCheckBlockShape b1t s1 "G"
         b1t <- f b1t
         b1 <- merge b1b b1t
         pmerge (b1,b2,b3,b4)

     -- 右にだけ張り付いている場合
     | x1' == x0' -> do
         (b1,b2,b3,b4) <- pcut parent bl1 -- 左下の点で分割
         (b3b, b3t) <- lcut b3 Y y1'
         debugCheckBlockShape b3b s1 "H"
         b3b <- f b3b
         b3 <- merge b3b b3t
         pmerge (b1,b2,b3,b4)

     -- どこにも張り付いていない場合
     | otherwise -> do
         (b1,b2,b3,b4) <- pcut parent bl1
         (b31,b32,b33,b34) <- pcut b3 tr1
         debugCheckBlockShape b31 s1 "I"
         b31 <- f b31
         b3 <- pmerge (b31,b32,b33,b34)
         pmerge (b1,b2,b3,b4)

fillRect :: BlockId -> Shape -> Color -> Paint BlockId
fillRect bid rect c =
  focus bid rect $ \bid' -> do
    color bid' c
    return bid'


sample_problem_1 = moves
  where
    (moves, cnt) = genMoves defaultInitialConfig $ \[block0] -> do
      let blue = (0,74,173,255)
          black = (0,0,0,255)
          white = (255,255,255,255)

      color block0 blue
      (b0bl, b0br, b0tr, b1) <- pcut block0 (40*9,40*1)
      (b1bl, b1br, b1tr, b1tl) <- pcut b1 (40*8,40*2)

      let cb88 blk = do
            color blk black
            (b1,b2,b3,b4) <- pcutRel blk (40*4,40*4)
            mapM_ cb44 [b1, b2, b3, b4]
          cb44 blk = do
            (b1,b2,b3,b4) <- pcutRel blk (40*2,40*2)
            mapM_ cb22 [b1, b2, b3, b4]
          cb22 blk = do
            (b1,b2,b3,b4) <- pcutRel blk (40,40)
            color b2 white
            color b4 white
      cb88 b1tl

      let g8 blk = do
            color blk black
            (bb, bt) <- lcutRel blk Y (40*4)
            mapM_ g4 [bb, bt]
          g4 blk = do
            (bb, bt) <- lcutRel blk Y (40*2)
            mapM_ g2 [bb, bt]
          g2 blk = do
            (_, bt) <- lcutRel blk Y 40
            color bt white
      g8 b1tr

      let h8 blk = do
            color blk black
            (bl, br) <- lcutRel blk X (40*4)
            mapM_ h4 [bl, br]
          h4 blk = do
            (bl, br) <- lcutRel blk X (40*2)
            mapM_ h2 [bl, br]
          h2 blk = do
            (bl, _) <- lcutRel blk X 40
            color bl white
      h8 b1bl

      return ()

sample_problem_2 = moves
  where
    (moves, cnt) = genMoves defaultInitialConfig $ \[block0] -> do
      -- 全体を縦に分割
      [_, bot, mid, top] <- lcutN block0 Y [50, 108, 265]

      -- 足部分
      [s1, leg1, s2, leg2, s3] <- lcutN bot X [138, 178, 228, 268]
      color leg1 (255,222,89,255)
      color leg2 (255,222,89,255)

      -- 胴体部分
      [s1, body, s2] <- lcutN mid X [102, 304]
      color body (56,182,255,255)  -- 水色
      [b1, belt, b2] <- lcutN body Y [122, 138]
      color belt (115,94,88,255)
      body <- mergeN [b1, belt, b2]
      body <- fillRect body (Rectangle (248,116) (277,145)) (131, 71, 124, 255)
      mid <- mergeN [s1, body, s2]
      -- 左の赤い手
      mid <- fillRect mid (Rectangle (76, 146) (115, 240)) (255, 22, 22, 255)

      -- 緑の手から頭にかけて
      mid_and_top <- merge mid top
      -- 緑の手
      mid_and_top <- fillRect mid_and_top (Rectangle (284,220) (322,313)) (0,128,55,255)

      -- 青い頭
      mid_and_top <- fillRect mid_and_top (Rectangle (139,241) (258,360)) (0,74,173,255)
      -- 口 (顔を一部塗り潰す)
      mid_and_top <- fillRect mid_and_top (Rectangle (156,267) (240,290)) (92,225,230,255)
      -- 顔の塗りつぶされた部分を塗り直す
      mid_and_top <- fillRect mid_and_top (Rectangle (175,282) (222,312)) (0,74,173,255)
      -- 左の緑の目
      mid_and_top <- fillRect mid_and_top (Rectangle (159,314) (177,335)) (126,217,87,255)
      -- 右の白い目
      mid_and_top <- fillRect mid_and_top (Rectangle (219,314) (237,335)) (255,255,255,255)

      return ()


sample_problem_3 :: IO [Move]
sample_problem_3 = do
  Right (ImageRGBA8 img) <- readImage "probs/3.png"
  let black = (0,0,0,255)
      white = (255,255,255,255)
      red = (255,22,22,255)
      gray = (216,219,219,255)
      yellow = (255,222,89,255)
      purple = (94,23,235,255)
      orange = (255,145,77,255)
      navy = (0,74,173,255)
      green1 = (201,226,101,255)
      green2 = (126,217,87,255)
      boxColors = [white, red, gray, yellow, purple, orange, navy, green1, green2]

  let findBoxes :: Color -> Int -> [(Int, Int, Int, Int)]
      findBoxes (r,g,b,a) eps = f [] ps
        where
          px = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
          ps = Set.fromList
               [ (x,y)
               | y <- [0..imageHeight img - 1], x <- [0..imageWidth img - 1]
               , let PixelRGBA8 r1 g1 b1 a1 = pixelAt img x y
               , let eps = 20
               , abs (fromIntegral r1 - r) < eps && abs (fromIntegral g1 - g) < eps && abs (fromIntegral b1 - b) < eps && abs (fromIntegral a1 - a) < eps
               ]
          f ret ps
            | Set.null ps = ret
            | otherwise = f (if w >= 2 && h >= 2 then (x0, y0, w, h) : ret else ret)
                            (ps Set.\\ Set.fromList [(x,y) | x<-[x0..x1], y<-[y0..y1]])
                where
                  (x0,y0) = Set.findMin ps
                  x1 = last $ x0 : takeWhile (\x' -> (x',y0) `Set.member` ps) [x0+1..]
                  y1 = last $ y0 : takeWhile (\y' -> all (\x' -> (x',y') `Set.member` ps) [x0..x1]) [y0+1..]
                  w = x1-x0+1
                  h = y1-y0+1

      findShapes :: Color -> Int -> [Shape]
      findShapes color eps =
        [Rectangle (x, imageHeight img - (y+h)) (x+w, imageHeight img - y)  | (x,y,w,h) <- findBoxes color eps]

  let (moves, cnt) = genMoves defaultInitialConfig $ \[block0] -> do
        color block0 black
        let m = forM_ boxColors $ \color -> do
                  forM_ (findShapes color 10) $ \rect -> do
                    bid <- get
                    bid <- lift $ fillRect bid rect color
                    put bid
        runStateT m block0

  return moves
