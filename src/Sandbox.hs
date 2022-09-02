module Sandbox where

import Data.Word

type BlockID = [Int]


data Color
  = RGBA { r :: Word8
         , g :: Word8
         , b :: Word8
         , a :: Word8
         } deriving (Show, Eq)

type Shape = Int

data SimpleBlock = SimpleBlock Shape Color
  deriving (Show, Eq)

data ComplexBlock = ComplexBlock Shape ChildBlocks
  deriving (Show, Eq)

type ChildBlocks = [Block]

type Block = Either SimpleBlock ComplexBlock
