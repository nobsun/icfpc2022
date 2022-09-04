module Content
  ( Content (..)
  , contentWidth
  , contentHeight
  , hmerge
  , vmerge
  , hsplit
  , vsplit
  , renderContentM
  ) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Control.Monad.Primitive

import Types (Color)


data Content
  = Fill !Int !Int Color
  | HMerge !Int Content Content
  | VMerge !Int Content Content
  deriving (Show)

contentWidth :: Content -> Int
contentWidth (Fill w _ _) = w
contentWidth (HMerge w _ _) = w
contentWidth (VMerge _ b _t) = contentWidth b

contentHeight :: Content -> Int
contentHeight (Fill _ h _) = h
contentHeight (HMerge _ l _r) = contentHeight l
contentHeight (VMerge h _ _) = h

hmerge :: Content -> Content -> Content
hmerge l r
  | contentHeight l /= contentHeight r = undefined
  | otherwise = HMerge (contentWidth l + contentWidth r) l r

vmerge :: Content -> Content -> Content
vmerge b t
  | contentWidth b /= contentWidth t = undefined
  | otherwise = VMerge (contentHeight b + contentHeight t) b t

hsplit :: Int -> Content -> (Content, Content)
hsplit s content
  | s <= 0 = undefined
  | contentWidth content <= s = undefined
  | otherwise =
      case content of
        Fill w h c -> (Fill s h c, Fill (w - s) h c)
        HMerge _ l r ->
          case compare s (contentWidth l) of
            EQ -> (l, r)
            LT -> case hsplit s l of (ll, lr) -> (ll, hmerge lr r)
            GT -> case hsplit (s - contentWidth l) r of (rl, rr) -> (hmerge l rl, rr)
        VMerge h b t ->
          case (hsplit s b, hsplit s t) of
            ((bl,br), (tl,tr)) -> (VMerge h bl tl, VMerge h br tr)

vsplit :: Int -> Content -> (Content, Content)
vsplit s content
  | s <= 0 = undefined
  | contentHeight content <= s = undefined
  | otherwise =
      case content of
        Fill w h c -> (Fill w s c, Fill w (h - s) c)
        HMerge w l r ->
          case (vsplit s l, vsplit s r) of
            ((lb,lt), (rb,rt)) -> (HMerge w lb rb, HMerge w lt rt)
        VMerge _ b t ->
          case compare s (contentHeight b) of
            EQ -> (b, t)
            LT -> case vsplit s b of (bb, bt) -> (bb, vmerge bt t)
            GT -> case vsplit (s - contentHeight b) t of (tb, tt) -> (vmerge b tb, tt)

renderContentM :: PrimMonad m => MutableImage (PrimState m) PixelRGBA8 -> Int -> Int -> Content -> m ()
renderContentM img = f
  where
    f x1 y1 (Fill w h (r,g,b,a)) = do
      let px = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
      forM_ [y1 .. y1 + h - 1] $ \y -> do
        forM_ [x1 .. x1 + w - 1] $ \x -> do
          writePixel img x (mutableImageHeight img - 1 - y) px
    f x1 y1 (HMerge s l r) = f x1 y1 l >> f (x1 + s) y1 r
    f x1 y1 (VMerge s b t) = f x1 y1 b >> f x1 (y1 + s) t
