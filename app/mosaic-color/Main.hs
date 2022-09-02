module Main where

import Codec.Picture
import Data.List
import System.Environment

import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

import Oga


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
  if depth == 1 then do
    programLineS (Move (MergeMove (BlockId (0:bid)) (BlockId (1:bid))))
    programLineS (Move (MergeMove (BlockId (2:bid)) (BlockId (3:bid))))
    B{bCounter=bCounter} <- get
    programLineS (Move (MergeMove (BlockId [bCounter-2]) (BlockId [bCounter-1])))
    modify (\b@B{bMerge=ms}-> b{bMerge=([bCounter]:ms)})
  else do
    B{bMerge=bMerge} <- get
    let (a:b:c:d:ms) = bMerge
    programLineS (Move (MergeMove (BlockId a) (BlockId b)))
    programLineS (Move (MergeMove (BlockId c) (BlockId d)))
    B{bCounter=bCounter} <- get
    programLineS (Move (MergeMove (BlockId [bCounter-2]) (BlockId [bCounter-1])))
    modify (\b-> b{bMerge=([bCounter]:ms)})




averageColor :: (Int,Int) -> (Int,Int) -> Image PixelRGBA8 -> PixelRGBA8
averageColor (bx,by) (tx,ty) img = p
  where
    p = (\(r,g,b,a) -> PixelRGBA8 (round (r/n)) (round (g/n)) (round (b/n)) (round (a/n))) $
        foldl' (\(r1,g1,b1,a1) (r2,g2,b2,a2) -> ((((,,,) $! (r1+r2)) $! (g1+g2)) $! (b1+b2)) $! (a1+a2))
          (0 :: Double, 0 :: Double, 0 :: Double, 0 :: Double)
          [ (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
          | y <- [max 0 (h-ty) .. min (h-1) (h-by)]
          , x <- [max 0 bx .. min (w-1) tx]
          , let PixelRGBA8 r g b a = pixelAt img x y
          ]
    n = fromIntegral ((tx-bx)*(ty-by))
    w = imageWidth img
    h = imageHeight img

main :: IO ()
main = do
  [fname] <- getArgs
  Right dynImg <- readImage fname
  case dynImg of
    ImageRGBA8 img -> do
      mapM_ print $ reverse $ bHistory $ execState (mosaicS 9 img [0]) initialBlock

