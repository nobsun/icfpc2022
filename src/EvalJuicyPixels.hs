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
import Control.Monad.Except
import Control.Monad.Primitive
import Control.Monad.RWS.Strict
import Control.Monad.ST
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import qualified Data.Vector.Generic as V

import Types


type M m = ExceptT String (RWST (MutableImage (PrimState m) PixelRGBA8) (Sum Integer) (Int, Map.Map BlockId Shape) m)


evalISL :: InitialConfig -> [Move] -> Either String (Image PixelRGBA8)
evalISL config moves = fmap fst $ evalISLWithCost config moves


evalISLWithCost :: InitialConfig -> [Move] -> Either String (Image PixelRGBA8, Integer)
evalISLWithCost config moves = runST $ do
  img <- createMutableImage (icWidth config) (icHeight config) (PixelRGBA8 255 255 255 255)

  forM_ (icBlocks config) $ \block -> do
    let (x1,y1) = icbBottomLeft block
        (x2,y2) = icbTopRight block
        (r,g,b,a) = icbColor block
        px = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
    forM_ [y1..y2-1] $ \y -> do
      forM_ [x1..x2-1] $ \x -> do
        writePixel img x (mutableImageHeight img - 1 - y) px

  let cnt = length (icBlocks config) - 1
      blocks =
        Map.fromList
        [ (V.singleton (read (icbBlockId block)), Rectangle (icbBottomLeft block) (icbTopRight block))
        | block <- icBlocks config
        ]

  (ret, _s, cost) <- runRWST (runExceptT (mapM_ evalMove moves)) img (cnt, blocks)
  case ret of
    Left err -> return (Left err)
    Right _ -> do
      img' <- unsafeFreezeImage img
      return (Right (img', getSum cost))


evalMove :: PrimMonad m => Move -> M m ()
evalMove (COLOR bid (r,g,b,a)) = do
  shape@(Rectangle (x,y) (x1,y1)) <- lookupBlock bid
  img <- ask
  -- αチャンネルを考慮して色を混ぜる必要はないようだ
  let px = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
  forM_ [y..y1-1] $ \y' -> do
    forM_ [x..x1-1] $ \x' -> do
      writePixel img x' (mutableImageHeight img - 1 - y') px
  canvasSize <- getCanvasSize
  addCost $ 5 * canvasSize / fromIntegral (shapeSize shape)
evalMove move@(LCUT bid orientation offset) = do
  shape@(Rectangle (x,y) (x1,y1)) <- lookupBlock bid
  (cnt, blocks) <- get
  case orientation of
    X -> do
      unless (x <= offset && offset <= x1) $
        throwError ("invalid move (" ++ dispMove move ++ "): " ++ show offset ++ " not in " ++ show x ++ ".." ++ show x1)
      put $
        ( cnt
        , Map.insert (V.snoc bid 0) (Rectangle (x,y) (offset,y1)) $
          Map.insert (V.snoc bid 1) (Rectangle (offset,y) (x1,y1)) $
          Map.delete bid blocks
        )
    Y -> do
      unless (y <= offset && offset <= y1) $
        throwError ("invalid move (" ++ dispMove move ++ "): " ++ show offset ++ " not in " ++ show y ++ ".." ++ show y1)
      put $
        ( cnt
        , Map.insert (V.snoc bid 0) (Rectangle (x,y) (x1,offset)) $
          Map.insert (V.snoc bid 1) (Rectangle (x,offset) (x1,y1)) $
          Map.delete bid blocks
        )
  canvasSize <- getCanvasSize
  addCost $ 7 * canvasSize / fromIntegral (shapeSize shape)
evalMove move@(PCUT bid (x1,y1)) = do
  (cnt, blocks) <- get
  shape@(Rectangle (x0,y0) (x2,y2)) <- lookupBlock bid
  unless (x0 <= x1 && x1 <= x2 && y0 <= y1 && y1 <= y2) $
    throwError ("invalid move (" ++ dispMove move ++ "): " ++ show (x1,y1) ++ " not in " ++ show shape)
  put $
    ( cnt
    , Map.union (Map.delete bid blocks) $
      Map.fromList
        [ (V.snoc bid 0, Rectangle (x0, y0) (x1, y1))
        , (V.snoc bid 1, Rectangle (x1, y0) (x2, y1))
        , (V.snoc bid 2, Rectangle (x1, y1) (x2, y2))
        , (V.snoc bid 3, Rectangle (x0, y1) (x1, y2))
        ]
    )
  canvasSize <- getCanvasSize
  addCost $ 10 * canvasSize / fromIntegral (shapeSize shape)
evalMove move@(SWAP bid1 bid2) = do
  (cnt, blocks) <- get
  shape1@(Rectangle (x1,y1) _) <- lookupBlock bid1
  shape2@(Rectangle (x2,y2) _) <- lookupBlock bid2
  unless (shapeSize shape1 == shapeSize shape2) $
    throwError ("invalid move (" ++ dispMove move ++ "): " ++ show (shapeSize shape1) ++ " /= " ++ show (shapeSize shape2))
  img <- ask
  forM_ [0 .. shapeHeight shape1 - 1] $ \i -> do
    forM_ [0 .. shapeWidth shape1 - 1] $ \j -> do
      px1 <- readPixel img (x1 + j) (mutableImageHeight img - 1 - (y1 + i))
      px2 <- readPixel img (x2 + j) (mutableImageHeight img - 1 - (y2 + i))
      writePixel img (x1 + j) (mutableImageHeight img - 1 - (y1 + i)) px2
      writePixel img (x2 + j) (mutableImageHeight img - 1 - (y2 + i)) px1
  put $
    ( cnt
    , Map.insert bid2 shape1 $ Map.insert bid1 shape2 $ blocks
    )
  canvasSize <- getCanvasSize
  addCost $ 3 * canvasSize / fromIntegral (shapeSize shape1)
evalMove move@(MERGE bid1 bid2) = do
  (cnt, blocks) <- get
  shape1@(Rectangle (x1,y1) (x1',y1')) <- lookupBlock bid1
  shape2@(Rectangle (x2,y2) (x2',y2')) <- lookupBlock bid2
  let blocks' = Map.delete bid1 $ Map.delete bid2 blocks
      cnt' = cnt + 1
      bid3 = V.singleton cnt'
  if x1 == x2 && shapeWidth shape1 == shapeWidth shape2 && (y1' == y2 || y2' == y1) then do
    put (cnt', Map.insert bid3 (Rectangle (x1, min y1 y2) (x1', max y1' y2')) blocks')
  else if y1 == y2 && shapeHeight shape1 == shapeHeight shape2 && (x1' == x2 || x2' == x1) then do
    put (cnt', Map.insert bid3 (Rectangle (min x1 x2, y1) (max x1' x2', y1')) blocks')
  else do
    throwError ("invalid move (" ++ dispMove move ++ ")")
  canvasSize <- getCanvasSize
  addCost $ 1 * canvasSize / fromIntegral (max (shapeSize shape1) (shapeSize shape2))


lookupBlock :: Monad m => BlockId -> M m Shape
lookupBlock bid = do
  (_, blocks) <- get
  case Map.lookup bid blocks of
    Nothing -> throwError ("no such block: " ++ dispBlockId bid)
    Just rect -> return rect


getCanvasSize :: Monad m => M m Double
getCanvasSize = do
  img <- ask
  return $ fromIntegral $ mutableImageWidth img * mutableImageHeight img


addCost :: Monad m => Double -> M m ()
addCost c = tell $ Sum (round' c)


sampleMoves =
  [ COLOR (V.fromList [0]) (0,74,173,255)
  , PCUT (V.fromList [0]) (360,40)
  , PCUT (V.fromList [0,3]) (320,80)
  , COLOR (V.fromList [0,3,3]) (0,0,0,255)
  , PCUT (V.fromList [0,3,3]) (160,240)
  --
  , PCUT (V.fromList [0,3,3,0]) (80,160)
  , PCUT (V.fromList [0,3,3,0,0]) (40,120)
  , COLOR (V.fromList [0,3,3,0,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,0,1]) (120,120)
  , COLOR (V.fromList [0,3,3,0,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,0,2]) (120,200)
  , COLOR (V.fromList [0,3,3,0,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,0,3]) (40,200)
  , COLOR (V.fromList [0,3,3,0,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,0,3,3]) (255,255,255,255)
  --
  , PCUT (V.fromList [0,3,3,1]) (240,160)
  , PCUT (V.fromList [0,3,3,1,0]) (200,120)
  , COLOR (V.fromList [0,3,3,1,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,1,1]) (280,120)
  , COLOR (V.fromList [0,3,3,1,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,1,2]) (280,200)
  , COLOR (V.fromList [0,3,3,1,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,1,3]) (200,200)
  , COLOR (V.fromList [0,3,3,1,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,1,3,3]) (255,255,255,255)
  --
  , PCUT (V.fromList [0,3,3,2]) (240,320)
  , PCUT (V.fromList [0,3,3,2,0]) (200,280)
  , COLOR (V.fromList [0,3,3,2,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,2,1]) (280,280)
  , COLOR (V.fromList [0,3,3,2,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,2,2]) (280,360)
  , COLOR (V.fromList [0,3,3,2,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,2,3]) (200,360)
  , COLOR (V.fromList [0,3,3,2,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,2,3,3]) (255,255,255,255)
  --
  , PCUT (V.fromList [0,3,3,3]) (80,320)
  , PCUT (V.fromList [0,3,3,3,0]) (40,280)
  , COLOR (V.fromList [0,3,3,3,0,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,0,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,3,1]) (120,280)
  , COLOR (V.fromList [0,3,3,3,1,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,1,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,3,2]) (120,360)
  , COLOR (V.fromList [0,3,3,3,2,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,2,3]) (255,255,255,255)
  , PCUT (V.fromList [0,3,3,3,3]) (40,360)
  , COLOR (V.fromList [0,3,3,3,3,1]) (255,255,255,255)
  , COLOR (V.fromList [0,3,3,3,3,3]) (255,255,255,255)
  --
  , COLOR (V.fromList [0,3,0]) (0,0,0,255)
  , LCUT (V.fromList [0,3,0]) X 160
  , LCUT (V.fromList [0,3,0,0]) X 80
  , LCUT (V.fromList [0,3,0,0,0]) X 40
  , COLOR (V.fromList [0,3,0,0,0,0]) (255,255,255,255)
  , LCUT (V.fromList [0,3,0,0,1]) X 120
  , COLOR (V.fromList [0,3,0,0,1,0]) (255,255,255,255)
  , LCUT (V.fromList [0,3,0,1]) X 240
  , LCUT (V.fromList [0,3,0,1,0]) X 200
  , COLOR (V.fromList [0,3,0,1,0,0]) (255,255,255,255)
  , LCUT (V.fromList [0,3,0,1,1]) X 280
  , COLOR (V.fromList [0,3,0,1,1,0]) (255,255,255,255)
  --
  , COLOR (V.fromList [0,3,2]) (0,0,0,255)
  , LCUT (V.fromList [0,3,2]) Y 240
  , LCUT (V.fromList [0,3,2,0]) Y 160
  , LCUT (V.fromList [0,3,2,0,0]) Y 120
  , COLOR (V.fromList [0,3,2,0,0,1]) (255,255,255,255)
  , LCUT (V.fromList [0,3,2,0,1]) Y 200
  , COLOR (V.fromList [0,3,2,0,1,1]) (255,255,255,255)
  , LCUT (V.fromList [0,3,2,1]) Y 320
  , LCUT (V.fromList [0,3,2,1,0]) Y 280
  , COLOR (V.fromList [0,3,2,1,0,1]) (255,255,255,255)
  , LCUT (V.fromList [0,3,2,1,1]) Y 360
  , COLOR (V.fromList [0,3,2,1,1,1]) (255,255,255,255)
  ]


test = do
  case evalISL defaultInitialConfig sampleMoves of
    Left err -> fail err
    Right img -> writePng "test.png" img


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
  case evalISLWithCost defaultInitialConfig sampleMoves of
    Left err -> fail err
    Right (img2, c) -> do
      print c
      print $ similarity img1 img1 == 0
      print $ similarity img2 img2 == 0
      print (similarity img1 img2)
      print $ similarity img1 img2 + c
