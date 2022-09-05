module Main where

import Data.Maybe
import System.Environment
import ISL
import Block
import Types
import World

main :: IO ()
main = do
     { args <- getArgs
     ; case args of
          []   -> putStrLn "stack exec -- gloss-display <ISLfile> [probrem-number]"
          a:as -> do
               { is <- map interp <$> loadISL a
               ; conf <- maybe (return defaultInitialConfig)
                               (loadInitialConfig . mkpath) (listToMaybe as)
               ; let { icanvas = Rectangle (0,0) (icWidth conf, icHeight conf)
                     ; itable  = mkBlockTable conf 
                     }    
               ; ipict <- mkPicture conf (listToMaybe as)
               ; glossDisplayWorld ipict
               . last
               . localEvalTrace 
               . initializeWorld icanvas itable
               . map interp =<< loadISL a
               }
     }

mkpath :: String -> FilePath
mkpath s = "probs/ini/"++s++".initial.json"

{-
main = glossDisplayWorld
     $ last $ localEvalTrace world0
-}