module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Lib.hs", "src/Sandbox.hs"]
