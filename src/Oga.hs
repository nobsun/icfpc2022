{-# OPTIONS_GHC -Wno-missing-fields #-}

module Oga where

import Control.Monad.State.Lazy
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Lazy as Map




test1 :: IO ()
test1 = do
  print $ execState (programS isl) initialBlock
  where
    isl = [Move (PCutMove (BlockId [0]) (Point 50 50))]

test2 :: IO ()
test2 = do
  print $ execState (programS isl) initialBlock
  where
    isl = [Move (PCutMove (BlockId [0]) (Point 50 50))
          ,Move (ColorMove (BlockId [0,0]) (Color 10 0 0 0))          ]

test3 :: IO ()
test3 = do
  print $ execState (programS isl) initialBlock
  where
    isl = [Move (PCutMove (BlockId [0]) (Point 50 50))
          ,Move (MergeMove (BlockId [1,0]) (BlockId [2,0]))
          ]


----------------------------------------


executeISL :: Program -> [Paint]
executeISL prog =
  sortBy (comparing pOrder) $ Map.elems paints
  where
    B{bPaints=paints} = execState (programS prog) initialBlock

initialBlock :: B
initialBlock = B
  { bBlocks  = Map.singleton [0] ((0,0),(400,400))
  , bOrder   = 0
  , bCounter = 1
  , bPaints  = Map.empty
  }

--------------------------------------

programS :: Program -> BState ()
programS prog =
  mapM_ programLineS prog

programLineS :: ProgramLine -> BState ()
programLineS (Comment s) = return ()
programLineS (Move m) = moveS m


moveS :: Move -> BState ()
moveS (PCutMove (BlockId bid) (Point x y)) = do
  B{bBlocks=bBlocks} <- get
  let ((bx,by),(tx,ty)) = bBlocks Map.! bid
      bBlocks' = foldr ($) bBlocks
        [Map.insert (0:bid) ((bx,by),(x,y))
        ,Map.insert (1:bid) ((x,by),(tx,y))
        ,Map.insert (2:bid) ((x,y),(tx,ty))
        ,Map.insert (3:bid) ((bx,y),(x,ty))
        ,Map.delete bid
        ]
  modify (\b-> b{bBlocks=bBlocks'})


moveS (LCutMove (BlockId bid) Vertical (LineNumber x)) = do
  B{bBlocks=bBlocks} <- get
  let ((bx,by),(tx,ty)) = bBlocks Map.! bid
      bBlocks' = foldr ($) bBlocks
        [Map.insert (0:bid) ((bx,by),(x,ty))
        ,Map.insert (1:bid) ((x,by),(tx,ty))
        ,Map.delete bid
        ]
  modify (\b-> b{bBlocks=bBlocks'})


moveS (ColorMove (BlockId bid) (Color r g b a)) = do
  B{bBlocks=bBlocks, bOrder=bOrder, bPaints=bPaints} <- get
  let (bl,tr) = bBlocks Map.! bid
      paint = Paint{pBlock=(bl,tr),pColor=(r,g,b,a),pBid=bid,pOrder=bOrder}
  modify (\b-> b{bOrder=(bOrder+1), bPaints=Map.insert bid paint bPaints})


moveS (SwapMove (BlockId bid1) (BlockId bid2)) = do
  B{bBlocks=bBlocks, bPaints=bPaints} <- get
  let Paint{pColor=p1} = bPaints Map.! bid1
      Paint{pColor=p2} = bPaints Map.! bid2
      bPaints' = foldr ($) bPaints
        [Map.update (\p->Just p{pColor=p2}) bid1
        ,Map.update (\p->Just p{pColor=p1}) bid2
        ]
  modify (\b-> b{bPaints=bPaints'})


moveS (MergeMove (BlockId bid1) (BlockId bid2)) = do
  B{bBlocks=bBlocks, bCounter=bCounter} <- get
  let (bl1,tr1) = bBlocks Map.! bid1
      (bl2,tr2) = bBlocks Map.! bid2
      bBlocks' = foldr ($) bBlocks
        [Map.insert [bCounter] (min bl1 bl2, max tr1 tr2)
        ,Map.delete bid1
        ,Map.delete bid2
        ]
  modify (\b-> b{bBlocks=bBlocks', bCounter=(bCounter+1)})



------------------------------------


data B = B
  { bBlocks  :: Map.Map [Int] (BottomLeft,TopRight)
  , bOrder   :: Int
  , bCounter :: Int
  , bPaints  :: Map.Map [Int] Paint
  }
  deriving Show

type BState a = State B a

type BottomLeft = (Int,Int)
type TopRight = (Int,Int)

data Paint = Paint
  { pBlock :: (BottomLeft,TopRight)
  , pColor :: (Int,Int,Int,Int)
  , pBid   :: [Int]
  , pOrder :: Int
  }
  deriving Show


--------------------------------------

type Program = [ProgramLine]

data ProgramLine
  = Comment String
  | Move Move
  deriving Show

data Move
  = PCutMove Block Point
  | LCutMove Block Orientation LineNumber
  | ColorMove Block Color
  | SwapMove Block Block
  | MergeMove Block Block
  deriving Show

data Orientation
  = Vertical
  | Horizontal
  deriving Show

data LineNumber = LineNumber Int
  deriving Show

data Block = BlockId [Int]
  deriving Show

data Point = Point Int Int
  deriving Show

data Color = Color Int Int Int Int
  deriving Show
