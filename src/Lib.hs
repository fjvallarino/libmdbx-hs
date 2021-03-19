module Lib (
  someFunc
) where

import Internal.LibMDX

someFunc :: IO ()
someFunc = putStrLn "someFunc"
