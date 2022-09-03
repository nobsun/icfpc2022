module Main where

import Codec.Picture
import Data.List
import System.Environment

import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import Oga





-----------------------------------

mosaicS :: Int -> Image PixelRGBA8 -> [Int] -> BState ()
mosaicS 0 img bid = do
  B{bBlocks=bBlocks} <- get
  let (bl,tr) = bBlocks Map.! bid
      PixelRGBA8 r g b a = averageColor bl tr img
  programLineS (Move (ColorMove (BlockId bid) (Color (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a))))

mosaicS depth img bid = do
  B{bBlocks=bBlocks} <- get
  let ((bx,by),(tx,ty)) = bBlocks Map.! bid
      (mx,my) = ((bx+tx)`div`2, (by+ty)`div`2)
  programLineS (Move (PCutMove (BlockId bid) (Point mx my)))
  mapM_ (mosaicS (depth-1) img) [0:bid, 1:bid, 2:bid, 3:bid]


averageColor :: (Int,Int) -> (Int,Int) -> Image PixelRGBA8 -> PixelRGBA8
averageColor (bx,by) (tx,ty) img = p
  where
    p = (\(n,r,g,b,a) -> PixelRGBA8 (round (r/n)) (round (g/n)) (round (b/n)) (round (a/n))) $
        foldl' (\(n1,r1,g1,b1,a1) (n2,r2,g2,b2,a2) -> ((((((,,,,) $! (n1+n2)) $! (r1+r2)) $! (g1+g2)) $! (b1+b2)) $! (a1+a2)))
          (0 :: Double, 0 :: Double, 0 :: Double, 0 :: Double, 0 :: Double)
          [ (1, fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
          | y <- [max 0 (h-ty) .. min (h-1) (h-by)]
          , x <- [max 0 bx .. min (w-1) tx]
          , let PixelRGBA8 r g b a = pixelAt img x y
          ]
    w = imageWidth img
    h = imageHeight img

------------------------------------

main :: IO ()
main = do
  [fname] <- getArgs
  Right dynImg <- readImage fname
  case dynImg of
    ImageRGBA8 img -> do
      mapM_ print $ reverse $ bHistory $ execState (mosaicS 6 img [0]) initialBlock

