{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PaintMonad
  ( genMoves

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
  ) where

import Control.Monad.RWS.Strict
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Vector.Generic as V

import Types

newtype E a = E (Either String a) deriving (Functor, Applicative, Monad)

instance MonadFail E where
  fail s = E (Left s)

type Paint = RWST Shape (Seq.Seq Move) Int E

genMoves :: InitialConfig -> ([BlockId] -> Paint a) -> ([Move], Int)
genMoves config f =
  case runRWST (f [icbBlockIdParsed block | block <- icBlocks config]) (Rectangle (0,0) (icWidth config, icHeight config)) (icCounter config) of
    E (Left err) -> error err
    E (Right (_, cnt', moves)) -> (F.toList moves, cnt')

emitMove :: Move -> Paint ()
emitMove move = tell (Seq.singleton move)

lcut :: BlockId -> Orientation -> Int -> Paint (BlockId, BlockId)
lcut bid dir offset = do
  emitMove $ LCUT bid dir offset
  return (V.snoc bid 0, V.snoc bid 1)

pcut :: BlockId -> Point -> Paint (BlockId, BlockId, BlockId, BlockId)
pcut bid point = do
  emitMove $ PCUT bid point
  return (V.snoc bid 0, V.snoc bid 1, V.snoc bid 2, V.snoc bid 3)

lcutRel :: BlockId -> Orientation -> Int -> Paint (BlockId, BlockId)
lcutRel bid dir offset = do
  Rectangle (x,y) _ <- ask
  lcut bid dir (if dir == X then x + offset else y + offset)

pcutRel :: BlockId -> Point -> Paint (BlockId, BlockId, BlockId, BlockId)
pcutRel bid (x,y) = do
  Rectangle (x0,y0) _ <- ask
  pcut bid (x0+x, y0+y)

lcutN :: BlockId -> Orientation -> [Int] -> Paint [BlockId]
lcutN bid dir [] = return [bid]
lcutN bid dir (x : xs) = do
  (b1, b2) <- lcut bid dir x
  bs <- lcutN b2 dir xs
  return (b1 : bs)

color :: BlockId -> Color -> Paint ()
color bid color = emitMove $ COLOR bid color

swap :: BlockId -> BlockId -> Paint ()
swap bid1 bid2 = emitMove $ SWAP bid1 bid2

merge :: BlockId -> BlockId -> Paint BlockId
merge bid1 bid2 = do
  emitMove $ MERGE bid1 bid2
  n <- get
  put $! n+1
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
focus parent (Rectangle bl tr) f = do
  -- TODO: 面積が0になる分割を行ってしまうことを避ける
  -- そのためにはブロックの座標をちゃんとトラックしていないといけない
  (b1,b2,b3,b4) <- pcut parent bl
  (b31,b32,b33,b34) <- pcut b3 tr
  b31 <- f b31
  b3 <- pmerge (b31,b32,b33,b34)
  pmerge (b1,b2,b3,b4)

fillRect :: BlockId -> Shape -> Color -> Paint BlockId
fillRect bid rect c =
  focus bid rect $ \bid' -> do
    color bid' c
    return bid'


sample_prob_2 = do
  mapM_ print moves
  -- writeFile "human_2.isl" (unlines (map show moves))

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
