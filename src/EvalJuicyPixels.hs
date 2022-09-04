module EvalJuicyPixels
  ( evalISL
  , evalISLWithCost
  , initialize
  , evalMove
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
  (img, s) <- initialize config
  (ret, _s, cost) <- runRWST (runExceptT (mapM_ evalMoveM moves)) img s
  case ret of
    Left err -> return (Left err)
    Right _ -> do
      img' <- unsafeFreezeImage img
      return (Right (img', getSum cost))


initialize :: PrimMonad m => InitialConfig -> m (MutableImage (PrimState m) PixelRGBA8, (Int, Map.Map BlockId Shape))
initialize config = do
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

  return (img, (cnt, blocks))


evalMove
  :: PrimMonad m
  => MutableImage (PrimState m) PixelRGBA8
  -> (Int, Map.Map BlockId Shape)
  -> Move
  -> m (Either String ((Int, Map.Map BlockId Shape), Integer))
evalMove img s move = do
  (ret, s', cost) <- runRWST (runExceptT (evalMoveM move)) img s
  case ret of
    Left err -> return (Left err)
    Right _ -> return (Right (s', getSum cost))


evalMoveM :: PrimMonad m => Move -> M m ()
evalMoveM (COLOR bid (r,g,b,a)) = do
  shape@(Rectangle (x,y) (x1,y1)) <- lookupBlock bid
  img <- ask
  -- αチャンネルを考慮して色を混ぜる必要はないようだ
  let px = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
  forM_ [y..y1-1] $ \y' -> do
    forM_ [x..x1-1] $ \x' -> do
      writePixel img x' (mutableImageHeight img - 1 - y') px
  canvasSize <- getCanvasSize
  addCost $ 5 * canvasSize / fromIntegral (shapeSize shape)
evalMoveM move@(LCUT bid orientation offset) = do
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
evalMoveM move@(PCUT bid (x1,y1)) = do
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
evalMoveM move@(SWAP bid1 bid2) = do
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
evalMoveM move@(MERGE bid1 bid2) = do
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
addCost c = tell $ Sum (roundJS c)


test = do
  case evalISL defaultInitialConfig sampleMoves of
    Left err -> fail err
    Right img -> writePng "test.png" img


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
