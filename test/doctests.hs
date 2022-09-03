module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Sandbox.hs"
               ,"src/ISL.hs"
               ]
