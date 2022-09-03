module EvalJuicyPixels
  ( evalISL
  , evalISLWithCost
  , similarity
  , pixelDiff
  ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.RWS
import Control.Monad.ST
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))

import Types


-- ((x,y), (w,h))
type Rect = ((Int,Int), (Int,Int))

type M m = RWST (MutableImage (PrimState m) PixelRGBA8) (Sum Integer) (Int, Map.Map BlockId Rect) m


evalISL :: (Int, Int) -> [Move] -> Image PixelRGBA8
evalISL size moves = fst $ evalISLWithCost size moves


evalISLWithCost :: (Int, Int) -> [Move] -> (Image PixelRGBA8, Integer)
evalISLWithCost (w,h) moves = runST $ do
  img <- createMutableImage w h (PixelRGBA8 255 255 255 255)
  (_, _s, cost) <- runRWST (mapM_ evalMove moves) img (0, Map.singleton [0] ((0,0),(w,h)))
  img' <- unsafeFreezeImage img
  return (img', getSum cost)


evalMove :: PrimMonad m => Move -> M m ()
evalMove (COLOR bid (r,g,b,a)) = do
  ((x,y), (w,h)) <- lookupBlock bid
  img <- ask
  -- TODO: αチャンネルを考慮して色を混ぜる必要がある?w
  let px = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
  forM_ [y..y+h-1] $ \y' -> do
    forM_ [x..x+w-1] $ \x' -> do
      writePixel img x' (mutableImageHeight img - 1 - y') px
  canvasSize <- getCanvasSize
  addCost $ 5 * canvasSize / fromIntegral (w * h)
evalMove (LCUT bid orientation offset) = do
  ((x,y), (w,h)) <- lookupBlock bid
  (cnt, blocks) <- get
  case orientation of
    X -> do
      unless (x <= offset && offset <= x + w) undefined
      put $
        ( cnt
        , Map.insert (0 : bid) ((x,y),(offset-x,h)) $
          Map.insert (1 : bid) ((offset,y),(x+w-offset,h)) $
          Map.delete bid blocks
        )
    Y -> do
      unless (y <= offset && offset <= y + h) undefined
      put $
        ( cnt
        , Map.insert (0 : bid) ((x,y),(w,offset-y)) $
          Map.insert (1 : bid) ((x,offset),(w,y+h-offset)) $
          Map.delete bid blocks
        )
  canvasSize <- getCanvasSize
  addCost $ 7 * canvasSize / fromIntegral (w * h)
evalMove (PCUT bid (x1,y1)) = do
  (cnt, blocks) <- get
  ((x,y), (w,h)) <- lookupBlock bid
  unless (x <= x1 && x1 <= x + w) undefined
  unless (y <= y1 && y1 <= y + h) undefined
  put $
    ( cnt
    , Map.union (Map.delete bid blocks) $
      Map.fromList
        [ (0 : bid, ((x,  y),  (x1-x,   y1-y)))
        , (1 : bid, ((x1, y),  (x+w-x1, y1-y)))
        , (2 : bid, ((x1, y1), (x+w-x1, y+h-y1)))
        , (3 : bid, ((x,  y1), (x1-x,   y+h-y1)))
        ]
    )
  canvasSize <- getCanvasSize
  addCost $ 10 * canvasSize / fromIntegral (w * h)
evalMove (SWAP bid1 bid2) = do
  (cnt, blocks) <- get
  ((x1,y1),size1@(w1,h1)) <- lookupBlock bid1
  ((x2,y2),size2@(w2,h2)) <- lookupBlock bid2
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
  canvasSize <- getCanvasSize
  addCost $ 3 * canvasSize / fromIntegral (w1 * h1)
evalMove (MERGE bid1 bid2) = do
  (cnt, blocks) <- get
  ((x1,y1),size1@(w1,h1)) <- lookupBlock bid1
  ((x2,y2),size2@(w2,h2)) <- lookupBlock bid2
  let blocks' = Map.delete bid1 $ Map.delete bid2 blocks
      cnt' = cnt + 1
      bid3 = [cnt']
  if x1 == x2 && w1 == w2 && (y1 + h1 == y2 || y2 + h2 == y1) then do
    put (cnt', Map.insert bid3 ((x1, min y1 y2), (w1, h1+h2)) blocks')
  else if y1 == y2 && h1 == h2 && (x1 + w1 == x2 || x2 + w2 == x1) then do
    put (cnt', Map.insert bid3 ((min x1 x2, y1), (w1+w2, h1)) blocks')
  else do
    undefined
  canvasSize <- getCanvasSize
  addCost $ 1 * canvasSize / fromIntegral (max (w1 * h1) (w2 * h2))


lookupBlock :: Monad m => BlockId -> M m Rect
lookupBlock bid = do
  (_, blocks) <- get
  case Map.lookup bid blocks of
    Nothing -> error ("no such block: " ++ dispBlockId bid)
    Just rect -> return rect


getCanvasSize :: Monad m => M m Double
getCanvasSize = do
  img <- ask
  return $ fromIntegral $ mutableImageWidth img * mutableImageHeight img


addCost :: Monad m => Double -> M m ()
addCost c = tell $ Sum (round' c)


sampleMoves =
  [ COLOR (reverse [0]) (0,74,173,255)
  , PCUT (reverse [0]) (360,40)
  , PCUT (reverse [0,3]) (320,80)
  , COLOR (reverse [0,3,3]) (0,0,0,255)
  , PCUT (reverse [0,3,3]) (160,240)
  --
  , PCUT (reverse [0,3,3,0]) (80,160)
  , PCUT (reverse [0,3,3,0,0]) (40,120)
  , COLOR (reverse [0,3,3,0,0,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,0,0,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,0,1]) (120,120)
  , COLOR (reverse [0,3,3,0,1,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,0,1,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,0,2]) (120,200)
  , COLOR (reverse [0,3,3,0,2,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,0,2,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,0,3]) (40,200)
  , COLOR (reverse [0,3,3,0,3,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,0,3,3]) (255,255,255,255)
  --
  , PCUT (reverse [0,3,3,1]) (240,160)
  , PCUT (reverse [0,3,3,1,0]) (200,120)
  , COLOR (reverse [0,3,3,1,0,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,1,0,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,1,1]) (280,120)
  , COLOR (reverse [0,3,3,1,1,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,1,1,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,1,2]) (280,200)
  , COLOR (reverse [0,3,3,1,2,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,1,2,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,1,3]) (200,200)
  , COLOR (reverse [0,3,3,1,3,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,1,3,3]) (255,255,255,255)
  --
  , PCUT (reverse [0,3,3,2]) (240,320)
  , PCUT (reverse [0,3,3,2,0]) (200,280)
  , COLOR (reverse [0,3,3,2,0,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,2,0,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,2,1]) (280,280)
  , COLOR (reverse [0,3,3,2,1,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,2,1,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,2,2]) (280,360)
  , COLOR (reverse [0,3,3,2,2,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,2,2,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,2,3]) (200,360)
  , COLOR (reverse [0,3,3,2,3,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,2,3,3]) (255,255,255,255)
  --
  , PCUT (reverse [0,3,3,3]) (80,320)
  , PCUT (reverse [0,3,3,3,0]) (40,280)
  , COLOR (reverse [0,3,3,3,0,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,3,0,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,3,1]) (120,280)
  , COLOR (reverse [0,3,3,3,1,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,3,1,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,3,2]) (120,360)
  , COLOR (reverse [0,3,3,3,2,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,3,2,3]) (255,255,255,255)
  , PCUT (reverse [0,3,3,3,3]) (40,360)
  , COLOR (reverse [0,3,3,3,3,1]) (255,255,255,255)
  , COLOR (reverse [0,3,3,3,3,3]) (255,255,255,255)
  --
  , COLOR (reverse [0,3,0]) (0,0,0,255)
  , LCUT (reverse [0,3,0]) X 160
  , LCUT (reverse [0,3,0,0]) X 80
  , LCUT (reverse [0,3,0,0,0]) X 40
  , COLOR (reverse [0,3,0,0,0,0]) (255,255,255,255)
  , LCUT (reverse [0,3,0,0,1]) X 120
  , COLOR (reverse [0,3,0,0,1,0]) (255,255,255,255)
  , LCUT (reverse [0,3,0,1]) X 240
  , LCUT (reverse [0,3,0,1,0]) X 200
  , COLOR (reverse [0,3,0,1,0,0]) (255,255,255,255)
  , LCUT (reverse [0,3,0,1,1]) X 280
  , COLOR (reverse [0,3,0,1,1,0]) (255,255,255,255)
  --
  , COLOR (reverse [0,3,2]) (0,0,0,255)
  , LCUT (reverse [0,3,2]) Y 240
  , LCUT (reverse [0,3,2,0]) Y 160
  , LCUT (reverse [0,3,2,0,0]) Y 120
  , COLOR (reverse [0,3,2,0,0,1]) (255,255,255,255)
  , LCUT (reverse [0,3,2,0,1]) Y 200
  , COLOR (reverse [0,3,2,0,1,1]) (255,255,255,255)
  , LCUT (reverse [0,3,2,1]) Y 320
  , LCUT (reverse [0,3,2,1,0]) Y 280
  , COLOR (reverse [0,3,2,1,0,1]) (255,255,255,255)
  , LCUT (reverse [0,3,2,1,1]) Y 360
  , COLOR (reverse [0,3,2,1,1,1]) (255,255,255,255)
  ]


test = writePng "test.png" $ evalISL (400, 400) sampleMoves


similarity :: Image PixelRGBA8 -> Image PixelRGBA8 -> Integer
similarity img1 img2 = assert (imageWidth img1 == imageWidth img2 && imageHeight img1 == imageHeight img2) $
  round' $ (alpha *) $ sum $
    [ pixelDiff p1 p2
    | y <- [0 .. imageHeight img1 - 1]
    , x <- [0 .. imageWidth img1 - 1]
    , let p1 = pixelAt img1 x y, let p2 = pixelAt img2 x y
    ]
  where
    alpha = 0.005


pixelDiff :: PixelRGBA8 -> PixelRGBA8 -> Double
pixelDiff (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  sqrt $ sum
    [ (fromIntegral r1 - fromIntegral r2)^(2::Int)
    , (fromIntegral g1 - fromIntegral g2)^(2::Int)
    , (fromIntegral b1 - fromIntegral b2)^(2::Int)
    , (fromIntegral a1 - fromIntegral a2)^(2::Int)
    ]


-- Haskell の round は偶数丸めだが、JavascriptのMath.roundは四捨五入なので、それに合わせる
-- https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/round
round' :: (RealFrac a, Integral b) => a -> b
round' x = floor (x + 0.5)


test_similarity = do
  Right (ImageRGBA8 img1) <- readImage "probs/1.png"
  let (img2, c) = evalISLWithCost (400, 400) sampleMoves
  print c
  print $ similarity img1 img1 == 0
  print $ similarity img2 img2 == 0
  print (similarity img1 img2)
  print $ similarity img1 img2 + c
