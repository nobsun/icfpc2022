module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.List
import System.Environment

import Control.Monad (forM_,when)
import Control.Monad.State.Lazy
import Data.Foldable (foldlM)
import qualified Data.Map.Lazy as Map

import Oga
import Types (pixelAtBT, readPixelBT)

--import Debug.Trace
--f $$ x = traceShow x (f x)


distance :: PixelRGBA8 -> PixelRGBA8 -> Int
distance (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  sum[((fromIntegral x)-(fromIntegral y))^2 | (x,y)<-[(r1,r2),(g1,g2),(b1,b2),(a1,a2)]]

------------------------------------------------

layerS :: Int -> Image PixelRGBA8 -> [Int] -> Int -> BState ()
layerS threshold img bid (-1) = return ()
layerS threshold img bid y = do
  B{bBlocks=bBlocks,bImage=bimg} <- get
  let ((bx,by),(tx,ty)) = bBlocks Map.! bid
  bid' <- foldlM (\bid' x -> do
      let color1 = pixelAtBT img x y
      color2 <- readPixelBT bimg x y
      if distance color1 color2 > (400`div`(y+1))*threshold then paint bid' x color1 else return bid'
    ) bid [bx..tx]
  programLineS(Move (LCutMove (BlockId bid') Horizontal (LineNumber y)))
  layerS threshold img (0:bid') (y-1)
  where
    paint bid' 0 (PixelRGBA8 r g b a) = do
      programLineS(Move (ColorMove (BlockId bid')(Color(fromIntegral r)(fromIntegral g)(fromIntegral b)(fromIntegral a))))
      return bid'
    paint bid' x (PixelRGBA8 r g b a) = do
      programLineS(Move (LCutMove (BlockId bid') Vertical (LineNumber x)))
      programLineS(Move (ColorMove (BlockId (1:bid'))(Color(fromIntegral r)(fromIntegral g)(fromIntegral b)(fromIntegral a))))
      programLineS(Move (MergeMove (BlockId (0:bid'))(BlockId (1:bid'))))
      B{bCounter=bCounter} <- get
      return [bCounter-1]

------------------------------------

main :: IO ()
main = do
  [threshold,fname] <- getArgs
  Right dynImg <- readImage fname
  case dynImg of
    ImageRGBA8 img -> do
      s <- execStateT (initS >> layerS (read threshold) img [0] 399) initialBlock
      mapM_ print $ reverse $ bHistory s
      freezeImage (bImage s) >>= writePng (fname++".layer.png")
