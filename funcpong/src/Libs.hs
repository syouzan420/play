module Libs where

isChar :: String -> Bool
isChar [] = True
isChar (x:xs) = let c = ['a'..'z']++['A'..'Z']
                 in (elem x c)&&(isChar xs)

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = let n = "0123456789+-"
                in (elem x n)&&(isNum xs)

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if(t==x) then 0 else 1+(getIndex t xs)
