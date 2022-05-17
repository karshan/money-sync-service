{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Util where

import System.Random
import Protolude
import qualified Data.Text as T
import Data.List ((!!))
import Data.Char (isAlpha, isSpace)
import Data.Time (Day, UTCTime (..))
import Data.Time.Format (defaultTimeLocale, parseTimeM)

parseBalance :: Text -> Either Text Int
parseBalance b = maybeToEither ("Failed to parse balance " <> b) . fmap dblUsd . readMaybe .
    toS . T.filter (\c -> c /= '$' && c /= ',') $ b

parseDate :: Text -> Either Text Day
parseDate dateS =
    maybeToEither ("parseDate \"" <> dateS <> "\" failed") $
        fmap utctDay $
        parseTimeM True defaultTimeLocale "%m/%d/%Y" $ toS dateS

dblUsd :: Double -> Int
dblUsd x = round (signum x) * round (abs x * 100)

stripExtraSpace :: Text -> Text
stripExtraSpace = T.intercalate " " . T.words . T.filter (\x -> isAlpha x || isSpace x) . T.toUpper

xTo0 :: Text -> Text
xTo0 = T.map (\x -> if x == 'X' then '0' else x)

isInfix :: Text -> Text -> Bool
isInfix x y = if T.length x >= T.length y then y `T.isInfixOf` x else x `T.isInfixOf` y

fuzzyMatch :: Text -> Text -> Bool
fuzzyMatch = isInfix `on` stripExtraSpace . xTo0

randomText :: RandomGen g => g -> Int -> (Text, g)
randomText _g = go (T.empty, _g)
    where
        charset = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['-', '_']
        go acc 0 = acc
        go (acc, g) n =
            let (c, nextG) = random g
            in go ((charset !! (c `mod` length charset)) `T.cons` acc, nextG) (n - 1)
