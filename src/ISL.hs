{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module ISL where

import qualified Data.Map as Map
import Data.Maybe
import Types
import Text.ParserCombinators.ReadP

rProgram :: ReadP [ProgLine]
rProgram = sepBy1 rProgramLine rNewline <* eof 

rNewline :: ReadP ProgLine
rNewline = Newline <$ char '\n'

rProgramLine :: ReadP ProgLine
rProgramLine = rNewline +++ rComment +++ (Move <$> rMove)

rComment :: ReadP ProgLine
rComment = Comment <$> (char '#' *> munch (/= '\n'))

--

fetch :: World -> (Instruction, World)
fetch world = ( head (prog world)
              , world { prog = tail (prog world)}
              )

interp :: ProgLine -> Instruction
interp pl world = case pl of
    Newline     -> world
    Comment msg -> world
    Move mv -> case mv of
        LCUT bid o off -> lcut bid o off world
        PCUT bid point -> pcut bid point world
        COLOR bid col  -> colormove bid col world
        SWAP bid1 bid2 -> swapmove bid1 bid2 world
        MERGE bid1 bid2 -> mergemove bid1 bid2 world


lcut :: BlockId -> Orientation -> Offset -> Instruction
lcut bid o off = \ case
    World { blocks  = tbl0
          } -> case o of
            X -> World { blocks = tbl2
                       }
        where
            (bid1, block1) = (0:bid, SimpleBlock shp1 col)
            (bid2, block2) = (1:bid, SimpleBlock shp2 col)
            block = tbl0 Map.! bid
            col = blockColor block
            (x0,y0) = leftBottom (shape block)
            (x1,y1) = rightUpper (shape block)
            (x00,y00,x01,y01,x10,y10,x11,y11) = case o of
                X -> (x0,y0,x0+off,y1,x0+off,y0,x1,y1)
                Y -> (x0,y0,x1,y0+off,x0,y0+off,x1,y1)
            shp1 = Rectangle { leftBottom = (x00,y00)
                             , rightUpper = (x01,y01)
                             }
            shp2 = Rectangle { leftBottom = (x10,y10)
                             , rightUpper = (x11,y11)
                             }
            tbl1 = Map.insert bid1 block1 tbl0
            tbl2 = Map.insert bid2 block2 tbl1



pcut :: BlockId -> Point -> Instruction
pcut = undefined

colormove = undefined
swapmove = undefined
mergemove = undefined

