{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Util where

import System.Random
import Protolude
import qualified Data.Text as T
import Data.List ((!!))

dblUsd :: Double -> Int
dblUsd x = floor (x * 100)

randomText :: RandomGen g => g -> Int -> (Text, g)
randomText _g = go (T.empty, _g)
    where
        charset = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['-', '_']
        go acc 0 = acc
        go (acc, g) n =
            let (c, nextG) = random g
            in go ((charset !! (c `mod` length charset)) `T.cons` acc, nextG) (n - 1)
