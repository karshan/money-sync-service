module Util where

import Data.Char (toLower)

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x:xs
lowerFirst [] = []

dblUsd :: Double -> Int
dblUsd x = floor (x * 100)
