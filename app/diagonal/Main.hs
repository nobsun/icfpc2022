module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.List (transpose)
import System.Environment

--import Control.Monad.Primitive (RealWorld)
import Control.Monad (forM_,when)
import Control.Monad.State.Lazy
import Data.Foldable (foldlM)
import qualified Data.Map.Lazy as Map

import Debug.Trace

import Oga

distance :: Integral a => [a] -> [a] -> Int
distance xs ys =
  round $ 0.005 * sqrt(sum ( zipWith f xs ys))
  where
    f x y = ((fromIntegral x)-(fromIntegral y))^2


pixelBoxAt :: (Int,Int) -> Int -> Image PixelRGBA8 -> [[Int]]
pixelBoxAt (x,y) size img =
  transpose $ map h [pixelAt img i (399-j)| i<-[x..x+size-1], j<-[y..y+size-1]]
  where
    h (PixelRGBA8 r g b a) = [fromIntegral r, fromIntegral g, fromIntegral b]

--readPixelBox :: (Int,Int) -> Int ->  MutableImage RealWorld PixelRGBA8 -> BState [[Int]]
readPixelBox (x,y) size bimg =
  fmap (transpose.map h) $ sequence[readPixel bimg i (399-j)| i<-[x..x+size-1], j<-[y..y+size-1]]
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

-- approximate cost
cost :: (Integral a, Num a) => (a,a) -> Int
cost (0,y') =
  round $ (7+1)* (160000/(max(y^2)((400-y)^2))) + 5*(160000/(400*(400-y)))
  where
    y = fromIntegral y'
cost (x',0) =
  round $ (7+1)* (160000/(max(x^2)((400-x)^2))) + 5*(160000/((400-x)*400))
  where
    x = fromIntegral x'
cost (x', y') =
  round $ 10 + 5*160000/((400-x)*(400-y)) + sum[max(cs!!i)(cs!!j) |(i,j)<-bs] + 200
  where
    x = fromIntegral x'
    y = fromIntegral y'
    cs = map (160000/) [x*y,(400-x)*y,(400-x)*(400-y),x*(400-y)]
    bs = case (x<200,x<y) of
          (True,True) -> [(1,0),(2,3)]
          (True,False)-> [(3,0),(2,1)]
          (False,True)-> [(1,2),(0,3)]
          _           -> [(3,2),(0,1)]



------------------------------------------------

diagonalS :: Int -> Image PixelRGBA8 -> [Int] -> BState [Int]
diagonalS size img bid = do
  foldlM (\bid' (x,y) -> do
      B{bImage=bimg} <- get
      let box = pixelBoxAt (x,y) size img
          color1 = map leastSquare box
          eff = sum $ zipWith distance box (map repeat color1)
      color2 <- fmap (map leastSquare) $ readPixelBox (x,y) size bimg
      if size*size*(distance color1 color2) > cost (x,y) + eff -- is it worth the cost?
        then paint bid' (x,y) color1
        else return bid'
    ) bid (concat[[(j,i-j)| j<-[0,size..i], 0<=i,i<=399,0<=j,j<=399]|i<-[0,size..2*399-size]])
  where
    paint :: [Int] -> (Int,Int) -> [Int] -> BState [Int]
    paint bid' (0,0) [r,g,b] = do
      programLineS(Move (ColorMove (BlockId bid')(Color(fromIntegral r)(fromIntegral g)(fromIntegral b)(fromIntegral 255))))
      return bid'
    paint bid' (x,y) [r,g,b] | x==0 || y==0 = do
      if x == 0 then
        programLineS(Move (LCutMove (BlockId bid') Horizontal (LineNumber y)))
      else
        programLineS(Move (LCutMove (BlockId bid') Vertical (LineNumber x)))
      programLineS(Move (ColorMove (BlockId (1:bid'))(Color(fromIntegral r)(fromIntegral g)(fromIntegral b)(fromIntegral 255))))
      programLineS(Move (MergeMove (BlockId (0:bid'))(BlockId (1:bid'))))
      B{bCounter=b} <- get
      return [b-1]
    paint bid' (x,y) [r,g,b] | otherwise = do
      programLineS(Move (PCutMove (BlockId bid')(Point x y)))
      programLineS(Move (ColorMove (BlockId (2:bid'))(Color(fromIntegral r)(fromIntegral g)(fromIntegral b)(fromIntegral 255))))
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
  [size,fname] <- getArgs
  Right dynImg <- readImage fname
  case dynImg of
    ImageRGBA8 img -> do
      s <- execStateT (initS >> diagonalS (read size) img [0]) initialBlock
      mapM_ print $ reverse $ bHistory s
      freezeImage (bImage s) >>= writePng (fname++".diagonal.png")
