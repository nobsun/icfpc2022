{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module PaintMonad.Samples
  ( sample_problem_1
  , sample_problem_2
  , sample_problem_3
  ) where

import Codec.Picture
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import qualified Data.Set as Set

import Types
import PaintMonad


-- saveISL "solution/human_1.isl" sample_problem_1
sample_problem_1 :: [Move]
sample_problem_1 = moves
  where
    (moves, _cnt) = genMoves defaultInitialConfig $ \[block0] -> do
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


-- saveISL "solution/human_2.isl" sample_problem_2
sample_problem_2 :: [Move]
sample_problem_2 = moves
  where
    (moves, _cnt) = genMoves defaultInitialConfig $ \[block0] -> do
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


-- saveISL "solution/human_3.isl" =<< sample_problem_3
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
          ps = Set.fromList
               [ (x,y)
               | y <- [0..imageHeight img - 1], x <- [0..imageWidth img - 1]
               , let PixelRGBA8 r1 g1 b1 a1 = pixelAt img x y
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

  let (moves, _cnt) = genMoves defaultInitialConfig $ \[block0] -> do
        color block0 black
        let m = forM_ boxColors $ \color -> do
                  forM_ (findShapes color 20) $ \rect -> do
                    bid <- get
                    bid <- lift $ fillRect bid rect color
                    put bid
        runStateT m block0

  return moves
