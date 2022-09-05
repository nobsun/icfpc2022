module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc"
               ,"src/Sandbox.hs"
               ,"src/ISL.hs"
               ,"src/Types.hs"
               ]
