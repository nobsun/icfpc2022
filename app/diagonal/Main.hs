module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.List
import System.Environment

import Control.Monad (forM_,when)
import Control.Monad.State.Lazy
import Data.Foldable (foldlM)
import qualified Data.Map.Lazy as Map

import Debug.Trace

import Oga


distance (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  ((f r1)-(f r2))^2 + ((f g1)-(f g2))^2 + ((f b1)-(f b2))^2
  where
    f = fromIntegral

------------------------------------------------

diagonalS :: Int -> Image PixelRGBA8 -> [Int] -> BState [Int]
diagonalS threshold img bid =
  foldlM (\bid' (x,y) -> do
      B{bImage=bimg} <- get
      let color1 = pixelAt img x (399-y)
      color2 <- readPixel bimg x y
      if distance color1 color2 > threshold then paint bid' (x,y) color1 else return bid'
    ) bid (concat[[(j,i-j)| j<-[0..i], 0<=i,i<=399,0<=j,j<=399]|i<-[0..2*399]])
  where
    paint :: [Int] -> (Int,Int) -> PixelRGBA8 -> BState [Int]
    paint bid' (x,y) (PixelRGBA8 r g b a) | x==0 || y==0 || x==399 || y==399 = do
      if x == 0 then
        programLineS(Move (LCutMove (BlockId bid') Vertical (LineNumber y)))
      else
        programLineS(Move (LCutMove (BlockId bid') Horizontal (LineNumber x)))
      programLineS(Move (ColorMove (BlockId (1:bid'))(Color(fromIntegral r)(fromIntegral g)(fromIntegral b)(fromIntegral a))))
      programLineS(Move (MergeMove (BlockId (0:bid'))(BlockId (1:bid'))))
      B{bCounter=b} <- get
      return [b-1]
    paint bid' (x,y) (PixelRGBA8 r g b a) | otherwise = do
      programLineS(Move (PCutMove (BlockId bid')(Point x y)))
      programLineS(Move (ColorMove (BlockId (2:bid'))(Color(fromIntegral r)(fromIntegral g)(fromIntegral b)(fromIntegral a))))
      let bs = case (x<200,x<y) of
                (True,True) -> [(1,0),(2,3)]
                (True,False)-> [(3,0),(2,1)]
                (False,True)-> [(1,2),(0,3)]
                _           -> [(3,2),(0,1)]
      forM_ bs (\(b1,b2) -> programLineS(Move (MergeMove (BlockId (b1:bid'))(BlockId (b2:bid')))))
      B{bCounter=b} <- get
      programLineS(Move (MergeMove (BlockId[b-1])(BlockId [b-2])))
      return [b]


------------------------------------

main :: IO ()
main = do
  [threshold,fname] <- getArgs
  Right dynImg <- readImage fname
  case dynImg of
    ImageRGBA8 img -> do
      s <- execStateT (initS >> diagonalS (read threshold) img [0]) initialBlock
      mapM_ print $ reverse $ bHistory s
      freezeImage (bImage s) >>= writePng (fname++".diagonal.png")
