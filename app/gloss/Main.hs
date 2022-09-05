module Main where

import System.Environment
import ISL
import Block
import Types

main :: IO ()
main = do
     { as <- getArgs
     ; case as of
          []  -> putStrLn "stack exec -- gloss-display <ISLfile>"
          a:_ -> glossDisplayWorld
               . last
               . localEvalTrace 
               . initializeWorld icanvas
               . map interp =<< loadISL a
     }

icanvas :: Canvas
icanvas = Rectangle (0,0) (400,400)

{-
main = glossDisplayWorld
     $ last $ localEvalTrace world0
-}