module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.List (foldl', transpose)
import System.Environment

import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import Oga

--import Debug.Trace
--f $$ x = traceShow x (f x)


distance :: PixelRGBA8 -> PixelRGBA8 -> Int
distance (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  sum[((fromIntegral x)-(fromIntegral y))^2 | (x,y)<-[(r1,r2),(g1,g2),(b1,b2),(a1,a2)]]


pixelBoxAt :: (Int,Int) -> Int -> Image PixelRGBA8 -> [[Int]]
pixelBoxAt (x,y) size img =
  transpose $ map h [pixelAt img i (399-j)| i<-[x..x+size-1], j<-[y..y+size-1]]
  where
    h (PixelRGBA8 r g b a) = [fromIntegral r, fromIntegral g, fromIntegral b]


leastSquare :: [Int] -> Int
leastSquare xs =
  if 0 <= center && center <= 255 then center
   else if 255^2 -b*255 > 0 then 0
        else 255
  where
    center = round $ -b/(2*a)
    a = fromIntegral $ length xs
    b = fromIntegral $ -2*(sum xs)

------------------------------------------------

-- できるだけpcutよりもlcutを使うようにしてみたがイマイチ
mosaic2S :: Int -> Image PixelRGBA8 -> [Int] -> BState ()
mosaic2S depth img bid = do
  B{bBlocks=bBlocks} <- get
  let ((bx,by),(tx,ty)) = bBlocks Map.! bid
      (mx,my) = ((bx+tx)`div`2, (by+ty)`div`2)
      dbx = distance (pixelAt img bx (399-by)) (pixelAt img tx (399-by))
      dtx = distance (pixelAt img tx (399-ty)) (pixelAt img bx (399-ty))
      dby = distance (pixelAt img bx (399-by)) (pixelAt img bx (399-ty))
      dty = distance (pixelAt img tx (399-ty)) (pixelAt img tx (399-by))
      vcut = depth>0 && mx/=bx && mx/=tx && (depth>7 || dbx > th || dtx > th)
      hcut = depth>0 && my/=by && my/=ty && (depth>7 || dby > th || dty > th)
  case (vcut, hcut) of
    (True,True) -> do
        programLineS (Move (PCutMove (BlockId bid) (Point mx my)))
        mapM_ (mosaic2S (depth-1) img) [0:bid, 1:bid, 2:bid, 3:bid]
    (True,False) -> do
        programLineS (Move (LCutMove (BlockId bid) Vertical (LineNumber mx)))
        mapM_ (mosaic2S (depth-1) img) [0:bid, 1:bid]
    (False,True) -> do
        programLineS (Move (LCutMove (BlockId bid) Horizontal (LineNumber my)))
        mapM_ (mosaic2S (depth-1) img) [0:bid, 1:bid]
    _ -> do
        let [r, g, b] = map leastSquare $ pixelBoxAt (bx,by) (tx-bx+1) img
        programLineS (Move (ColorMove (BlockId bid) (Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255)))
  where
    th = 20000


-----------------------------------

-- 単純にpcutしていく
mosaicS :: Int -> Image PixelRGBA8 -> [Int] -> BState ()
mosaicS 0 img bid = do
  B{bBlocks=bBlocks} <- get
  let ((bx,by),(tx,ty)) = bBlocks Map.! bid
      [r,g,b] = map leastSquare $ pixelBoxAt (bx,by) (tx-bx+1) img
  programLineS (Move (ColorMove (BlockId bid) (Color (fromIntegral r) (fromIntegral g) (fromIntegral b) 255)))

mosaicS depth img bid = do
  B{bBlocks=bBlocks} <- get
  let ((bx,by),(tx,ty)) = bBlocks Map.! bid
      (mx,my) = ((bx+tx)`div`2, (by+ty)`div`2)
  programLineS (Move (PCutMove (BlockId bid) (Point mx my)))
  mapM_ (mosaicS (depth-1) img) [0:bid, 1:bid, 2:bid, 3:bid]


------------------------------------

main :: IO ()
main = do
  [n,fname] <- getArgs
  Right dynImg <- readImage fname
  case dynImg of
    ImageRGBA8 img -> do
      s <- execStateT (initS >> mosaicS (read n) img [0]) initialBlock
--      s <- execStateT (initS >> mosaic2S 10 img [0]) initialBlock
      mapM_ print $ reverse $ bHistory s
      rawImage <- freezeImage $ bImage s
      writePng (fname++".mosaic.png") rawImage

