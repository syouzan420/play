module StrToInt(makeIntList) where

import Data.Char (isDigit)

makeIntList :: String -> [Integer]
makeIntList str = let strs = words str
                      isInts = map isInt strs
                   in if and isInts then map (read.elimPlus) strs else []

isInt :: String -> Bool
isInt str = let str' = if head str=='-' || head str=='+' then tail str else str
             in foldr ((&&).isDigit) True str'

elimPlus :: String -> String
elimPlus [] = []
elimPlus (x:xs) = if x=='+' then xs else x:xs
