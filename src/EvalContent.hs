module EvalContent
  ( evalISL
  , evalISLWithCost
  , initialState
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
import Content

-- ------------------------------------------------------------------------

data CanvasState
  = CanvasState
  { canvasCounter :: !Int
  , canvasBlocks :: Map.Map BlockId (Point, Content)
  }


type M = ExceptT String (RWS (Int, Int) (Sum Integer) CanvasState)


evalISL :: InitialConfig -> [Move] -> Either String (Image PixelRGBA8)
evalISL config moves = fmap fst $ evalISLWithCost config moves


evalISLWithCost :: InitialConfig -> [Move] -> Either String (Image PixelRGBA8, Integer)
evalISLWithCost config moves = do
  case runRWS (runExceptT (mapM_ evalMoveM moves)) (icWidth config, icHeight config) (initialState config) of
    (ret, CanvasState _ blocks, cost) -> do
      _ <- ret
      return (renderBlocks (icWidth config) (icHeight config) blocks, getSum cost)


initialState :: InitialConfig -> CanvasState
initialState config =
  CanvasState
  { canvasCounter = icCounter config
  , canvasBlocks =
      Map.fromList
      [ (icbBlockIdParsed block, (icbBottomLeft block, maybe noColor (Fill (x1-x0) (y1-y0)) $ icbColor block))
      | block <- icBlocks config
      , let (x0,y0) = icbBottomLeft block
      , let (x1,y1) = icbTopRight block
      , let noColor = error "EvalContent.initialState: icbPngBottomLeftPoint not implemented"
      ]
  }


renderBlocks :: Int -> Int -> Map.Map BlockId (Point, Content) -> Image PixelRGBA8
renderBlocks w h blocks = runST $ do
  img <- createMutableImage w h (PixelRGBA8 255 255 255 255)
  forM_ (Map.elems blocks) $ \((x,y), content) -> renderContentM img x y content
  unsafeFreezeImage img


evalMove :: Int -> Int -> Move -> CanvasState -> Either String (CanvasState, Integer)
evalMove w h move s = do
  case runRWS (runExceptT (evalMoveM move)) (w,h) s of
    (ret, s', cost) -> do
      _ <-  ret
      return (s', getSum cost)


evalMoveM :: Move -> M ()
evalMoveM (COLOR bid color) = do
  (pos, content) <- lookupBlock bid
  -- αチャンネルを考慮して色を混ぜる必要はないようだ
  modifyBlocks $ Map.insert bid (pos, Fill (contentWidth content) (contentHeight content) color)
  canvasSize <- getCanvasSize
  addCost $ 5 * canvasSize / fromIntegral (contentSize content)
evalMoveM move@(LCUT bid orientation offset) = do
  ((x,y), content) <- lookupBlock bid
  modifyBlocks $ Map.delete bid
  case orientation of
    X -> do
      unless (x <= offset && offset <= x + contentWidth content) $
        throwError ("invalid move (" ++ dispMove move ++ "): " ++ show offset ++ " not in " ++ show x ++ ".." ++ show (x + contentWidth content))
      case hsplit (offset - x) content of
        (l, r) -> do
          modifyBlocks $ Map.insert (V.snoc bid 0) ((x,y), l)
          modifyBlocks $ Map.insert (V.snoc bid 1) ((offset,y), r)
    Y -> do
      unless (y <= offset && offset <= y + contentHeight content) $
        throwError ("invalid move (" ++ dispMove move ++ "): " ++ show offset ++ " not in " ++ show y ++ ".." ++ show (y + contentHeight content))
      case vsplit (offset - y) content of
        (b, t) -> do
          modifyBlocks $ Map.insert (V.snoc bid 0) ((x,y), b)
          modifyBlocks $ Map.insert (V.snoc bid 1) ((x,offset), t)
  canvasSize <- getCanvasSize
  addCost $ 7 * canvasSize / fromIntegral (contentSize content)
evalMoveM move@(PCUT bid (x1,y1)) = do
  ((x0,y0), content) <- lookupBlock bid
  let (x2,y2) = (x0 + contentWidth content, y0 + contentHeight content)
  modifyBlocks $ Map.delete bid
  unless (x0 <= x1 && x1 <= x2 && y0 <= y1 && y1 <= y2) $
    throwError ("invalid move (" ++ dispMove move ++ "): " ++ show (x1,y1) ++ " not in " ++ show ((x0,y0),(x2,y2)))
  case vsplit (y1 - y0) content of
    (b, t) ->
      case (hsplit (x1 - x0) b, hsplit (x1 - x0) t) of
        ((bl,br), (tl,tr)) ->
          modifyBlocks $
            Map.insert (V.snoc bid 0) ((x0, y0), bl) .
            Map.insert (V.snoc bid 1) ((x1, y0), br) .
            Map.insert (V.snoc bid 2) ((x1, y1), tr) .
            Map.insert (V.snoc bid 3) ((x0, y1), tl)
  canvasSize <- getCanvasSize
  addCost $ 10 * canvasSize / fromIntegral (contentSize content)
evalMoveM move@(SWAP bid1 bid2) = do
  -- CanvasState cnt blocks <- get
  block1@(pos1, content1) <- lookupBlock bid1
  block2@(pos2, content2) <- lookupBlock bid2
  unless ((contentWidth content1, contentHeight content1) == (contentWidth content2, contentHeight content2)) $
    throwError ("invalid move (" ++ dispMove move ++ "): " ++ show (contentWidth content1, contentHeight content1) ++ " /= " ++ show (contentWidth content2, contentHeight content2))
  modifyBlocks $
    Map.insert bid1 block2 .
    Map.insert bid2 block1
  canvasSize <- getCanvasSize
  addCost $ 3 * canvasSize / fromIntegral (contentSize content1)
evalMoveM move@(MERGE bid1 bid2) = do
  CanvasState cnt blocks <- get
  ((x1,y1), content1) <- lookupBlock bid1
  ((x2,y2), content2) <- lookupBlock bid2
  let (x1',y1') = (x1 + contentWidth content1, y1 + contentHeight content1)
      (x2',y2') = (x2 + contentWidth content2, y2 + contentHeight content2)
  let blocks' = Map.delete bid1 $ Map.delete bid2 blocks
      cnt' = cnt + 1
      bid3 = V.singleton cnt'
  put (CanvasState cnt' blocks')
  if x1 == x2 && contentWidth content1 == contentWidth content2 && (y1' == y2 || y2' == y1) then do
    modifyBlocks $ Map.insert bid3 ((x1, min y1 y2), if y1 < y2 then vmerge content1 content2 else vmerge content2 content1)
  else if y1 == y2 && contentHeight content1 == contentHeight content2 && (x1' == x2 || x2' == x1) then do
    modifyBlocks $ Map.insert bid3 ((min x1 x2, y1), if x1 < x2 then hmerge content1 content2 else hmerge content2 content1)
  else do
    throwError ("invalid move (" ++ dispMove move ++ ")")
  canvasSize <- getCanvasSize
  addCost $ 1 * canvasSize / fromIntegral (max (contentSize content1) (contentSize content2))


lookupBlock :: BlockId -> M (Point, Content)
lookupBlock bid = do
  CanvasState _ blocks <- get
  case Map.lookup bid blocks of
    Nothing -> throwError ("no such block: " ++ dispBlockId bid)
    Just x -> return x


modifyBlocks :: (Map.Map BlockId (Point, Content) -> Map.Map BlockId (Point, Content)) -> M ()
modifyBlocks f = modify (\canvas -> canvas{ canvasBlocks = f (canvasBlocks canvas) })


getCanvasSize :: M Double
getCanvasSize = do
  (w,h) <- ask
  return $ fromIntegral $ w*h


addCost :: Double -> M ()
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
