module Main where

import Data.Maybe
import System.Environment
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Juicy as Gloss

main :: IO ()
main = do 
    { args <- getArgs
    ; case args of
        []  -> putStrLn "pngview <png-file>"
        a:_ -> do { img <- Gloss.loadJuicyPNG a
                  ; Gloss.display (window a) Gloss.black (fromMaybe Gloss.blank img)
                  }
    }

window :: String -> Gloss.Display
window t = Gloss.InWindow t (960, 960) (400,400)