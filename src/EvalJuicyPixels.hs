module EvalJuicyPixels
  ( evalISL
  ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.RWS
import Control.Monad.ST
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map

import Types

-- TODO: コスト計算

-- ((x,y), (w,h))
type Rect = ((Int,Int), (Int,Int))

type M m = RWST (MutableImage (PrimState m) PixelRGBA8) () (Int, Map.Map BlockId Rect) m


evalISL :: (Int, Int) -> [Move] -> Image PixelRGBA8
evalISL (w,h) moves = runST $ do
  img <- createMutableImage w h (PixelRGBA8 255 255 255 255)
  (_, _s, _w) <- runRWST (mapM_ evalMove moves) img (0, Map.singleton [0] ((0,0),(w,h)))
  unsafeFreezeImage img


evalMove :: PrimMonad m => Move -> M m ()
evalMove (COLOR bid (r,g,b,a)) = do
  (_cnt, blocks) <- get
  case Map.lookup bid blocks of
    Nothing -> error ("no such block: " ++ dispBlockId bid)
    Just ((x,y), (w,h)) -> do
      img <- ask
      -- TODO: αチャンネルを考慮して色を混ぜる必要がある?
      let px = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
      forM_ [y..y+h-1] $ \y' -> do
        forM_ [x..x+w-1] $ \x' -> do
          writePixel img x' (mutableImageHeight img - 1 - y') px
evalMove (LCUT bid orientation offset) = do
  (cnt, blocks) <- get
  case Map.lookup bid blocks of
    Nothing -> error ("no such block: " ++ dispBlockId bid)
    Just ((x,y), (w,h)) -> do
      case orientation of
        X -> do
          unless (x <= offset && offset <= x + w) undefined
          put $
            ( cnt
            , Map.insert (bid ++ [0]) ((x,y),(offset-x,h)) $
              Map.insert (bid ++ [1]) ((offset,y),(x+w-offset,h)) $
              Map.delete bid blocks
            )
        Y -> do
          unless (y <= offset && offset <= y + h) undefined
          put $
            ( cnt
            , Map.insert (bid ++ [0]) ((x,y),(w,offset-y)) $
              Map.insert (bid ++ [1]) ((x,offset),(w,y+h-offset)) $
              Map.delete bid blocks
            )
evalMove (PCUT bid (x1,y1)) = do
  (cnt, blocks) <- get
  case Map.lookup bid blocks of
    Nothing -> error ("no such block: " ++ dispBlockId bid)
    Just ((x,y),(w,h)) -> do
      unless (x <= x1 && x1 <= x + w) undefined
      unless (y <= y1 && y1 <= y + h) undefined
      put $
        ( cnt
        , Map.union (Map.delete bid blocks) $
          Map.fromList
            [ (bid ++ [0], ((x,  y),  (x1-x,   y1-y)))
            , (bid ++ [1], ((x1, y),  (x+w-x1, y1-y)))
            , (bid ++ [2], ((x1, y1), (x+w-x1, y+h-y1)))
            , (bid ++ [3], ((x,  y1), (x1-x,   y+h-y1)))
            ]
        )
evalMove (SWAP bid1 bid2) = do
  (cnt, blocks) <- get
  case Map.lookup bid1 blocks of
    Nothing -> error ("no such block: " ++ dispBlockId bid1)
    Just ((x1,y1),size1@(w1,h1)) -> do
      case Map.lookup bid2 blocks of
        Nothing -> error ("no such block: " ++ dispBlockId bid2)
        Just ((x2,y2),size2@(w2,h2)) -> do
          unless (size1 == size2) undefined
          img <- ask
          forM_ [0..h1-1] $ \i -> do
            forM_ [0..w1-1] $ \j -> do
              px1 <- readPixel img (x1 + j) (mutableImageHeight img - 1 - (y1 + i))
              px2 <- readPixel img (x2 + j) (mutableImageHeight img - 1 - (y2 + i))
              writePixel img (x1 + j) (mutableImageHeight img - 1 - (y1 + i)) px2
              writePixel img (x2 + j) (mutableImageHeight img - 1 - (y2 + i)) px1
          put $
            ( cnt
            , Map.insert bid2 ((x1,y1),size1) $ Map.insert bid1 ((x2,y2),size2) $ blocks
            )
evalMove (MERGE bid1 bid2) = do
  (cnt, blocks) <- get
  case Map.lookup bid1 blocks of
    Nothing -> error ("no such block: " ++ dispBlockId bid1)
    Just ((x1,y1),size1@(w1,h1)) -> do
      case Map.lookup bid2 blocks of
        Nothing -> error ("no such block: " ++ dispBlockId bid2)
        Just ((x2,y2),size2@(w2,h2)) -> do
          let blocks' = Map.delete bid1 $ Map.delete bid2 blocks
              cnt' = cnt + 1
              bid3 = [cnt']
          if x1 == x2 && w1 == w2 && (y1 + h1 == y2 || y2 + h2 == y1) then do
            put (cnt', Map.insert bid3 ((x1, min y1 y2), (w1, h1+h2)) blocks')
          else if y1 == y2 && h1 == h2 && (x1 + w1 == x2 || x2 + w2 == x1) then do
            put (cnt', Map.insert bid3 ((min x1 x2, y1), (w1+w2, h1)) blocks')
          else do
            undefined


sampleMoves =
  [ COLOR [0] (0,74,173,255)
  , PCUT [0] (360,40)
  , PCUT [0,3] (320,80)
  , COLOR [0,3,3] (0,0,0,255)
  , PCUT [0,3,3] (160,240)
  --
  , PCUT [0,3,3,0] (80,160)
  , PCUT [0,3,3,0,0] (40,120)
  , COLOR [0,3,3,0,0,1] (255,255,255,255)
  , COLOR [0,3,3,0,0,3] (255,255,255,255)
  , PCUT [0,3,3,0,1] (120,120)
  , COLOR [0,3,3,0,1,1] (255,255,255,255)
  , COLOR [0,3,3,0,1,3] (255,255,255,255)
  , PCUT [0,3,3,0,2] (120,200)
  , COLOR [0,3,3,0,2,1] (255,255,255,255)
  , COLOR [0,3,3,0,2,3] (255,255,255,255)
  , PCUT [0,3,3,0,3] (40,200)
  , COLOR [0,3,3,0,3,1] (255,255,255,255)
  , COLOR [0,3,3,0,3,3] (255,255,255,255)
  --
  , PCUT [0,3,3,1] (240,160)
  , PCUT [0,3,3,1,0] (200,120)
  , COLOR [0,3,3,1,0,1] (255,255,255,255)
  , COLOR [0,3,3,1,0,3] (255,255,255,255)
  , PCUT [0,3,3,1,1] (280,120)
  , COLOR [0,3,3,1,1,1] (255,255,255,255)
  , COLOR [0,3,3,1,1,3] (255,255,255,255)
  , PCUT [0,3,3,1,2] (280,200)
  , COLOR [0,3,3,1,2,1] (255,255,255,255)
  , COLOR [0,3,3,1,2,3] (255,255,255,255)
  , PCUT [0,3,3,1,3] (200,200)
  , COLOR [0,3,3,1,3,1] (255,255,255,255)
  , COLOR [0,3,3,1,3,3] (255,255,255,255)
  --
  , PCUT [0,3,3,2] (240,320)
  , PCUT [0,3,3,2,0] (200,280)
  , COLOR [0,3,3,2,0,1] (255,255,255,255)
  , COLOR [0,3,3,2,0,3] (255,255,255,255)
  , PCUT [0,3,3,2,1] (280,280)
  , COLOR [0,3,3,2,1,1] (255,255,255,255)
  , COLOR [0,3,3,2,1,3] (255,255,255,255)
  , PCUT [0,3,3,2,2] (280,360)
  , COLOR [0,3,3,2,2,1] (255,255,255,255)
  , COLOR [0,3,3,2,2,3] (255,255,255,255)
  , PCUT [0,3,3,2,3] (200,360)
  , COLOR [0,3,3,2,3,1] (255,255,255,255)
  , COLOR [0,3,3,2,3,3] (255,255,255,255)
  --
  , PCUT [0,3,3,3] (80,320)
  , PCUT [0,3,3,3,0] (40,280)
  , COLOR [0,3,3,3,0,1] (255,255,255,255)
  , COLOR [0,3,3,3,0,3] (255,255,255,255)
  , PCUT [0,3,3,3,1] (120,280)
  , COLOR [0,3,3,3,1,1] (255,255,255,255)
  , COLOR [0,3,3,3,1,3] (255,255,255,255)
  , PCUT [0,3,3,3,2] (120,360)
  , COLOR [0,3,3,3,2,1] (255,255,255,255)
  , COLOR [0,3,3,3,2,3] (255,255,255,255)
  , PCUT [0,3,3,3,3] (40,360)
  , COLOR [0,3,3,3,3,1] (255,255,255,255)
  , COLOR [0,3,3,3,3,3] (255,255,255,255)
  --
  , COLOR [0,3,0] (0,0,0,255)
  , LCUT [0,3,0] X 160
  , LCUT [0,3,0,0] X 80
  , LCUT [0,3,0,0,0] X 40
  , COLOR [0,3,0,0,0,0] (255,255,255,255)
  , LCUT [0,3,0,0,1] X 120
  , COLOR [0,3,0,0,1,0] (255,255,255,255)
  , LCUT [0,3,0,1] X 240
  , LCUT [0,3,0,1,0] X 200
  , COLOR [0,3,0,1,0,0] (255,255,255,255)
  , LCUT [0,3,0,1,1] X 280
  , COLOR [0,3,0,1,1,0] (255,255,255,255)
  --
  , COLOR [0,3,2] (0,0,0,255)
  , LCUT [0,3,2] Y 240
  , LCUT [0,3,2,0] Y 160
  , LCUT [0,3,2,0,0] Y 120
  , COLOR [0,3,2,0,0,1] (255,255,255,255)
  , LCUT [0,3,2,0,1] Y 200
  , COLOR [0,3,2,0,1,1] (255,255,255,255)
  , LCUT [0,3,2,1] Y 320
  , LCUT [0,3,2,1,0] Y 280
  , COLOR [0,3,2,1,0,1] (255,255,255,255)
  , LCUT [0,3,2,1,1] Y 360
  , COLOR [0,3,2,1,1,1] (255,255,255,255)
  ]


test = writePng "test.png" $ evalISL (400, 400) sampleMoves
