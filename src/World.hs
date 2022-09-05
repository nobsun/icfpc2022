{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module World where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Vector as V

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Juicy as GlossJ

import Types
import Block

data World
    = World
    { canvas      :: Shape
    , prog        :: [Instruction]
    , counter     :: Int
    , blocks      :: BlockTable
    , picture     :: Gloss.Picture
    , costs       :: !Int
    }

instance Show World where
    show w = case w of
        World { blocks = tbl }
            -> unlines (map (dispBlockEntry tbl) (Map.assocs tbl))

initializeWorld :: Canvas -> BlockTable -> [Instruction] -> World
initializeWorld can tab is
    = World
    { canvas = can
    , prog = is
    , counter = 1
    , blocks = tab -- Map.singleton (V.singleton 0) (SimpleBlock can white)
    , picture = undefined
    , costs  = 0
    }

white :: Color
white = (255,255,255,255)

gray, red, green, blue :: Color
gray = (0,0,0,0)
red  = (255,0,0,255)
green = (0,255,0,255)
blue  = (0,0,255,255)

incCount :: World -> (Int, World)
incCount world = (cnt, world { counter = succ cnt })
    where
        cnt = counter world

type Instruction = World -> World

glossDisplayWorld :: Gloss.Picture -> World -> IO ()
glossDisplayWorld pict world 
    = do
    { Gloss.display window Gloss.white $ (pict <>)
    $ Gloss.translate dx dy $ foldr1 (<>) $ map blockToGlossPicture $ Map.elems $ blocks world
    }
    where
        window = Gloss.FullScreen
        can   = canvas world
        (dx,dy) = ( fromIntegral $ negate $ halve $ shapeWidth can
                  , fromIntegral $ negate $ halve $ shapeHeight can
                  )

mkBlockTable :: InitialConfig -> BlockTable
mkBlockTable conf = Map.fromList $ map conv (icBlocks conf)
    where
        conv icb = let
            { bid = V.singleton (read (icbBlockId icb))
            ; col = fromMaybe (0,0,0,0) (icbColor icb)
            ; b = SimpleBlock (Rectangle (icbBottomLeft icb) (icbTopRight icb)) col
            } in (bid, b)

mkPicture :: InitialConfig -> Maybe String -> IO Gloss.Picture
mkPicture conf = maybe (return Gloss.blank) (mkPict conf . mkPngPath)

mkPngPath :: String -> FilePath
mkPngPath = ("probs/ini/src/"++) . (++".source.png")

mkPict :: InitialConfig -> FilePath -> IO Gloss.Picture
mkPict conf f = do
    { img <- GlossJ.loadJuicyPNG f 
    ; case img of
        Nothing -> return Gloss.blank
        Just pict -> 
            return $ foldl phi Gloss.blank
                   $ mapMaybe icbPngBottomLeftPoint (icBlocks conf)
            where
                phi pic (x,y) = pic <> Gloss.translate (fromIntegral x) (fromIntegral y) pict
    }
